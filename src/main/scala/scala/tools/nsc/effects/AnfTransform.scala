package scala.tools.nsc.effects

trait AnfTransform { self: EffectDomain =>
  import global._
  import analyzer.Typer

  /**
   * Notes
   *
   * - need anf to avoid unsoundness. example:
   *
   *   def f(a: A) = () => a    // f: (a: A) -> (() => Unit) { def apply(): Unit @loc(a) }
   *   val t = f(new A)         // should have type (() => Unit) { def apply(): Unit @loc(any) }
   *
   *
   * - the local values should be pushed up as much as possible to avoid imprecise results. example:
   *
   *   // (hc: HasCounter) -> ((c: Counter) => HasCounter) { def apply(c: Counter): HasCounter @mod(hc,c) @loc(hc)
   *   def setCounter(hc: HasCounter) = (c: Counter) => (hc.counter = c; hc)
   *
   *   def t = {
   *     val f = setCounter(new HasCounter)
   *     val f.apply(new Counter)
   *   }
   *
   *   def tAnf1 = {
   *     // f has type ((c: Counter) => HasCounter) { def apply(c: Counter): HasCounter @mod(any) @loc(any)
   *     val f = setCounter({val x$1 = new HasCounter; x$1})
   *     f.apply({val x$2 = new Counter; x$2})                 // @mod(any) @loc(any)
   *   }
   *
   *   def tAnf2 = {
   *     val x$1 = new HasCounter
   *     // f has type ((c: Counter) => HasCounter) { def apply(c: Counter): HasCounter @mod(x$1, c) @loc(x$1)
   *     val f = setCounter(x$1)
   *     val x$2 = new Counter
   *     f.apply(x$2)                                          // @mod(x$2) @loc(x$2)  -> pure, fresh
   *   }
   *
   *
   * - pushing up is not straightforward
   *
   *   foo(bar(baz()))
   *     => foo({val a = baz(); bar(a)})                  // ok..
   *     => val b = { val a = baz(); bar(a)}; foo(b)      // would like to have "val a" before "val b", not inside.
   *     => val a = baz(); val b = bar(a); foo(b)         // need some normalization
   *
   *
   *
   *
   *
   *
   * Implementation issues
   *
   * - flattening blocks (ie the normalization required for the above example) might lead to name clashes.
   *   maybe solution: only flatten out synthetic values?
   *
   *   val x = 1
   *   foo({val x = 2; f()})
   *
   *   => val x = 1
   *      val x$1 = {val x = 2; f()}
   *      foo(x$1)
   *
   *   => val x = 1
   *      val x = 2         // problem!
   *      val x$1 = f()
   *      foo(x$1)
   *
   * - If we move an argument to a local ValDef, and the argument is a block with some definitions, then the owner
   *   chain needs to be fixed to include the new ValDef. Use resetAllAttrs (or something similar)?
   *
   * - moving out an argument to a local value can make the result not type-check if there is widening:
   *
   *   def f(x: <specific-singleton-type>) = ...
   *   f(getX())
   *
   *   =>  val x$1 = getX()  // has a widened type
   *       f(x$1)            // might therefore not type check
   *
   */

  object AnfTransformer /*extends Transformer*/ {

    def transformToAnf(tree: Tree, typer: Typer, pt: Type): Tree = {
      if (tree.isErroneous) tree
      else {
        val anfTree = mkBlock(transformToList(tree))
        val res = typer.typed(anfTree, pt)

        if (EffectChecker.traceAnf) {
          val strBefore = tree.toString
          val strAfter  = res.toString
          if (strBefore.length == strAfter.length) res
          else EffectChecker.printRes(res, s"anf of $strBefore --- ")
        } else {
          res
        }
      }
    }


    private lazy val Boolean_ShortCircuits: Set[Symbol] = {
      import definitions.BooleanClass
      def BooleanTermMember(name: String) = BooleanClass.typeSignature.member(newTermName(name).encodedName)
      val Boolean_&& = BooleanTermMember("&&")
      val Boolean_|| = BooleanTermMember("||")
      Set(Boolean_&&, Boolean_||)
    }

    def isByName(fun: Tree): ((Int, Int) => Boolean) = {
      if (Boolean_ShortCircuits contains fun.symbol) (i, j) => true
      else {
        val byNamess = fun.tpe.paramss.map(_.map(_.isByNameParam))
        (i, j) => util.Try(byNamess(i)(j)).getOrElse(false)
      }
    }

    def argName(fun: Tree): ((Int, Int) => String) = {
      val namess = fun.tpe.paramss.map(_.map(_.name.toString))
      (i, j) => util.Try(namess(i)(j)).getOrElse(s"arg_${i}_${j}")
    }

    case class Arg(expr: Tree, isByName: Boolean, argName: String)

    private def mapArguments[A](args: List[Tree])(f: (Tree, Int) => (A, Tree)): (List[A], List[Tree]) = {
      args match {
        case args :+ Typed(tree, Ident(tpnme.WILDCARD_STAR)) =>
          val (a, argExprs :+ lastArgExpr) = (args :+ tree).zipWithIndex.map(f.tupled).unzip
          val exprs = argExprs :+ Typed(lastArgExpr, Ident(tpnme.WILDCARD_STAR)).setPos(lastArgExpr.pos)
          (a, exprs)
        case args                                            =>
          args.zipWithIndex.map(f.tupled).unzip
      }
    }

    def mapArgumentss[A](fun: Tree, argss: List[List[Tree]])(f: Arg => (A, Tree)): (List[List[A]], List[List[Tree]]) = {
      val isByNamess: (Int, Int) => Boolean = isByName(fun)
      val argNamess: (Int, Int) => String = argName(fun)
      argss.zipWithIndex.map { case (args, i) =>
        mapArguments[A](args) {
          (tree, j) => f(Arg(tree, isByNamess(i, j), argNamess(i, j)))
        }
      }.unzip
    }

    private def isTrivial(arg: Tree) = arg match {
      case Ident(_) => true
      case Literal(_) => true
    }

    def freshName(prefix: String): String = {
      currentUnit.fresh.newName(prefix+"$")
    }

    private def mkVal(prefix: String, rhs: Tree, pos: Position): ValDef = {
      val vd = ValDef(NoMods, freshName(prefix), TypeTree(), rhs)
      vd.setPos(pos)
      vd
    }

    def mkBlock(trees: List[Tree]): Tree = trees match {
      case List(expr) => expr
      case stats :+ expr => Block(stats, expr).setPos(expr.pos)
    }
    
    def transformToList(tree: Tree): List[Tree] = tree match {

      case Select(qual, name) =>
        val stats :+ expr = transformToList(qual)
        if (expr eq qual) List(tree)
        else stats :+ Select(expr, name).setPos(tree.pos)

      case treeInfo.Applied(fun, targs, argss) if argss.nonEmpty =>
        // TODO: conserve Apply if everything is in ANF
        val funStats :+ simpleFun = transformToList(fun)
        val (argStatss, argExprss): (List[List[List[Tree]]], List[List[Tree]]) =
          mapArgumentss[List[Tree]](fun, argss) {
            case Arg(arg, _, _) if treeInfo.isExprSafeToInline(arg) =>
              (Nil, arg)

            case Arg(arg, byName, _) if byName =>
              // @TODO: is that ok? Infer.scala handles by name arguments
              (Nil, mkBlock(transformToList(arg)))

            case Arg(arg, _, argName) =>
              val (stats :+ expr) = transformToList(arg)
              val valDef = mkVal(argName, expr, arg.pos)
              (stats :+ valDef, Ident(valDef.name).setPos(arg.pos))
          }
        val core = if (targs.isEmpty) simpleFun else TypeApply(simpleFun, targs).setPos(tree.pos)
        val newApply = argExprss.foldLeft(core)(Apply(_, _))
        val r = funStats ++ argStatss.flatten.flatten :+ newApply.setPos(tree.pos)
        r

      case Block(stats, expr) =>
        (stats :+ expr) flatMap transformToList

      case ValDef(mods, name, tpt, rhs) if !mods.isLazy =>
        val stats :+ expr = transformToList(rhs)
        if (expr eq rhs) {
          List(tree)
        } else {
          // calling `setSymbol` is important because (at least for now) we don't reset and re-type all
          // of the already typed trees. so the already typed trees can refer to the symbol that was initially
          // created for that ValDef. if we just insert an empty one here the typer will create a new symbol
          // for it and things get out of sync. For ValDefs that already have a symbol the typer just uses that
          // instead of creating a new one.
          val anfVd = ValDef(mods, name, tpt, expr).setSymbol(tree.symbol)
          stats :+ anfVd.setPos(tree.pos)
        }

      case Assign(lhs, rhs) =>
        val stats :+ expr = transformToList(rhs)
        if (expr eq rhs) List(tree)
        else stats :+ Assign(lhs, expr).setPos(tree.pos)

      case If(cond, thenp, elsep) =>
        val condStats :+ condExpr = transformToList(cond)

        val thenStats :+ thenExpr = transformToList(thenp)
        val elseStats :+ elseExpr = transformToList(elsep)
        if ((condExpr eq cond) && (thenExpr eq thenp) && (elseExpr eq elsep)) List(tree)
        else {
          val thenBlock = mkBlock(thenStats :+ thenExpr)
          val elseBlock = mkBlock(elseStats :+ elseExpr)
          condStats :+ If(condExpr, thenBlock, elseBlock).setPos(tree.pos)
        }


      case Match(scrut, cases) =>
        val scrutStats :+ scrutExpr = transformToList(scrut)
        val caseDefs = transformCases(cases)
        if ((scrutExpr eq scrut) && caseDefs.zip(cases).forall(p => p._1 eq p._2)) List(tree)
        else {
          scrutStats :+ Match(scrutExpr, caseDefs).setPos(tree.pos)
        }


      case LabelDef(name, params, rhs) =>
        val rhsStats :+ rhsExpr = transformToList(rhs)
        if (rhsExpr eq rhs) List(tree)
        else List(LabelDef(name, params, mkBlock(rhsStats :+ rhsExpr)).setSymbol(tree.symbol))

      case TypeApply(fun, targs) =>
        val funStats :+ simpleFun = transformToList(fun)
        if (simpleFun eq fun) List(tree)
        else funStats :+ TypeApply(simpleFun, targs).setPos(tree.pos)

      case Try(block, catches, finalizer) =>
        val tryStats :+ tryExpr = transformToList(block)
        val caseDefs = transformCases(catches)
        val finStats :+ finExpr = transformToList(finalizer)
        if ((tryExpr eq block) && caseDefs.zip(catches).forall(p => p._1 eq p._2) && (finExpr eq finalizer)) {
          List(tree)
        } else {
          val tryBlock = mkBlock(tryStats :+ tryExpr)
          val finalizerBlock = mkBlock(finStats :+ finExpr)
          List(Try(tryBlock, caseDefs, finalizerBlock).setPos(tree.pos))
        }

      case Throw(expr) =>
        val throwStats :+ throwExpr = transformToList(expr)
        if (throwExpr eq expr) List(tree)
        else throwStats :+ Throw(throwExpr).setPos(tree.pos)

      case Typed(expr, tpt) =>
        val tpdStats :+ tpdExpr = transformToList(expr)
        if (tpdExpr eq expr) List(tree)
        else tpdStats :+ Typed(tpdExpr, tpt).setPos(tree.pos)

      case Return(expr) =>
        val retStats :+ retExpr = transformToList(expr)
        if (retExpr eq expr) List(tree)
        else retStats :+ Return(retExpr).setPos(tree.pos)

      case _ =>
        List(tree)
    }

    def transformCases(cases: List[CaseDef]): List[CaseDef] = cases map {
      case cd @ CaseDef(pat, guard, body) =>
        val guardStats :+ guardExpr = transformToList(guard)
        val bodyStats :+ bodyExpr = transformToList(body)
        if ((guardExpr eq guard) && (bodyExpr eq body)) {
          cd
        } else {
          val guardBlock = mkBlock(guardStats :+ guardExpr)
          val bodyBlock = mkBlock(bodyStats :+ bodyExpr)
          CaseDef(pat, guardBlock, bodyBlock).setPos(cd.pos)
        }
    }
  }
}
