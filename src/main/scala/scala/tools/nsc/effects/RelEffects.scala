package scala.tools.nsc.effects

trait RelEffects { self: EffectDomain =>
  import global._
  import lattice._

  lazy val relClass = rootMirror.getClassByName(newTypeName("scala.annotation.effects.rel"))
  lazy val percent = {
    val effPackage = rootMirror.getModuleByName(newTermName("scala.annotation.effects"))
    definitions.getMember(effPackage, newTermName("%").encode)
  }

  /**
   * Returns the relative effects of a method
   * @TODO: document difference to the method relFromAnnotation
   *
   * The relative effect of a method is defined as follows:
   *  - If the method has an explicit return type, the @rel annotations on the return type (if
   *    there are none, the method doesn't have a relative effect)
   *  - If the return type of the method is inferred, the relative effect is that of the next
   *    enclosing method.
   *
   * The implementation of this method is very tricky. The reason is that we cannot find out from
   * a method symbol if it has an inferred type or not.
   *
   *
   * Scenario 1
   * ----------
   *
   *   def foo = {
   *     def bar = 1
   *     bar
   *   }
   *
   * Events (explained in text below)
   *
   *  + fooSym.info                       // Somebody wants the type of `foo`
   *  |-+ computeType(fooBody)            // Namers.typeSig(foo), type completer for foo
   *  | |-+ Namers.enterSym(barSym)       // Typers.typedBlock, statements
   *  | |-+ typed(reference-to-bar)       // Typers.typedBlock, expr
   *  |   |-+ barSym.info                 // Typer asks the type of `barSym`
   *  |   | |-- computeType(barBody)      // Namers.typeSig(bar), type completer for bar
   *  |   | |-+ pluginsTypeSig(bar)       // Finished computing the type of `barBody`, call pluginsTypeSig before assigning to `barSym`
   *  |   | | |-+ computeEffect(barBody)
   *  |   | |   |-- relEffects(barSym)
   * -|---|-|------------------------------- future
   *  |   | |-+ barSym.setType            // replaces the lazy type of barSym
   *  |   |
   *  |   |-+ assign type to reference-to-bar
   *  |
   *  |-+ pluginsTypeSig(foo)
   *  | |-+ computeEffect(fooBody)
   *  |   |-+ relEffects(fooSym)
   *  |
   *  |--+ fooSym.setType                 // replaces the lazy type of fooSym
   *
   *
   *
   * Inferring the type of `foo` triggers type inference for the type of `bar`. This means that in
   * the analyzer plugin, we will first receive a call to `pluginsTypeSig` for symbol `bar`, once
   * its type has been computed by `typeSig` - `bar` is the method whose type is computed first.
   *
   * Since the type of `bar` was inferred, the effects plugin will also infer the effects of the body
   * of `bar`. To do that, we need to know what are the relative effects of `bar` (this information is
   * passed to effect inference in the effect typing context).
   *
   * Therefore there will be an invocation `relEffects(barSym)`. At this point, the symbol for `bar`
   * still has a lazy type: the inferred type will only be assigned to `barSym` *after* `pluginsTypeSig`
   * returns a result. Also the symbol of `foo` will still have a lazy type: the type inference for
   * the body of `foo` is still running.
   *
   * The fact that these two symbols have lazy types when we arrive here indicates that they have an
   * inferred type, which means they also have an inferred effect (if we want to change that, i.e. support
   * an @inferEff annotation, we need to fix things).
   *
   * That explains: if the method symbol passed into `relEffects` has a lazy type, we use the relative
   * effects of the next enclosing method.
   *
   *
   * Scenario 2
   * ----------
   *
   *   def foo: Int = {
   *     def bar = 1
   *     bar
   *   }
   *
   *  + fooSym.info                       // Somebody wants the type of `foo`
   *  |-+ typedType(Int).tpe              // Namers.typeSig(foo)
   *  |-+ pluginsTypeSig(foo)             // call plugins before
   *  | |-+ ---nothing---                 // since type was not inferred, also effects are not inferred. if the return
   *  |                                      type had effect annots they would end up in the resulting type of course.
   *  |-+ fooSym.setType                  // replaces the lazy type
   *
   * --------------------------------------- later, during typer phase
   *
   *  + typedDefDef(foo)
   *  |-+ fooSym.initialize               // if nobody asked for `foo.info` before, will do the above. nothing otherwise
   *  |-+ typed(fooBody)
   *  | |-+ Namers.enterSym(barSym)
   *  | |-+ typed(reference-to-bar)
   *  |   |-+ barSym.info
   *  |   | |-- computeType(barBody)
   *  |   | |-+ pluginsTypeSig(bar)
   *  |   | | |-+ computeEffect(barBody)
   *  |   | |   |-- relEffects(barSym)    // DIFFERENCE TO BEFORE: the enclosing fooSym now has a non-lazy type!!!
   * -|---|-|------------------------------- future
   *  |   | |-+ barSym.setType
   *  |   |
   *  |   |-+ assign type to reference-to-bar
   *  |
   *  |-+ verify that fooBody.tpe <:< foo.tpt
   *
   *
   * Note that `fooSym` *also* has a lazy type initially! Instead of type-checking the body of `foo`,
   * this lazy type will type check the expected type and not touch the body tree.
   *
   * What happens in this case: the body tree of `foo` will only be type-checked during the typer phase.
   * Typing `fooBody` is still the same as before, up to the call to `relEffects`: now the inner `barSym`
   * has a lazy type (we're computing its type / effect) BUT the enclosing `fooSym` has a given type. So
   * `relEffects` will return the `@rel` annotations that are found on `fooSym` (which are none in the exmaple).
   *
   * ------------------
   *
   * INVARIANTS: We make a few assumptions here
   *
   *   1. The completer of an enclosing method is *always* invoked *before* the completer of a nested method.
   *      There are two cases how the inner completer can be invoked:
   *        a. Due to type inference of the enclosing method (first scenario). Then obviously the inner completer
   *           is called later (i.e. within the outer completer)
   *        b. Due to type-checking the outer DefDef. Since typedDefDef calls `initialize` in the beginning, the
   *           outer completer is executed first.
   *
   *   2. An inferred return type always means an inferred effect.
   *
   * ------------------
   *
   * If we want to change the second assumption, i.e. infer effects when there is an explicit returnt type, a
   * type-check of a body is triggered even if there's an explicit return type. Before that (in pluginsTypeSig),
   * we have the possibility to add an attachment to the outer method symbol and provide some information which
   * could be extracted here. So we could recognize these symbols here and maybe handle them differently.
   *
   *   def foo: Int @infer @rel(x) = { ... }
   *
   * Would trigger effect inference. But before, that, we could assign attach the `@rel` effect to `fooSym`. Then
   * we could still use the annotated relative effect, even thought the `fooSym` still has a lazy type.
   *
   * ------------------
   *
   * Constructor Symbols
   *
   */
  def relEffects(sym: Symbol): List[RelEffect] =
    if (sym == NoSymbol) {
      List()
    } else if (!sym.isMethod && !sym.isLazy) {
      abort(s"expected method when looking up relative effects, got $sym")
    } else if (sym.rawInfo.isComplete) {
      relFromAnnotation(sym.tpe)
    } else {
      relEffects(sym.owner.enclMethod)
    }

  /**
   * The relative effect of the (return type of the potential method type) `tpe`.
   */
  def relFromAnnotation(tpe: Type): List[RelEffect] = {
    relFromAnnotationList(tpe.finalResultType.annotations)
  }

  def relFromAnnotationList(annots: List[AnnotationInfo]): List[RelEffect] = {
    val relAnnots = annots.filter(_.atp.typeSymbol == relClass)
    (List[RelEffect]() /: relAnnots)((eff, annot) =>
      joinRel(eff, readRelAnnot(annot))
    )
  }

  /**
   * Convert a @rel annotation (which might have multiple arguments) to a list of RelEffects
   */
  private def readRelAnnot(relAnn: AnnotationInfo): List[RelEffect] = {
    def paramFun(tree: Tree): (Loc, Option[Symbol]) = tree match {
      case TypeApply(fun, targs) =>
        paramFun(fun)
      case Apply(fun, args) =>
        paramFun(fun)
      case f @ Select(p @ Ident(_), _) =>
        (ParamLoc(p.symbol), Some(f.symbol))
      case f @ Select(t @ This(_), _) =>
        (ThisLoc(t.symbol), Some(f.symbol))
      case p @ Ident(_) =>
        (ParamLoc(p.symbol), None)
      case t @ This(_) =>
        (ThisLoc(t.symbol), None)
      case _ =>
        abort("unexpected tree in @rel annotation: "+ tree)
    }
    relAnn.args.map(arg => {
      val (param, fun) = paramFun(arg)
      RelEffect(param, fun)
    })
  }

  def relToAnnotation(eff: List[RelEffect]): List[AnnotationInfo] = {
    if (eff.isEmpty) Nil
    else {
      val args = eff map(e => {
        val par = e.param match {
          case ThisLoc(c) => This(c)
          case ParamLoc(p) => Ident(p)
        }
        e.fun match {
          case None =>
            par
          case Some(f) =>
            f.paramss.foldLeft[Tree](Select(par, f))(
              // (fun, params) => Apply(fun, params map (p => Typed(Ident(percent), TypeTree(p.tpe))))
              (fun, params) => Apply(fun, params map (_ => Ident(percent)))
            )
        }
      })
      // trees need types for pickling. types should be correct, else later phases might crash
      // (refchecks), so running typer. need to travel to typer phase for that, otherwise we can
      // end up with an assertion failure ("silent mode is not available past typer")
      //
      // in 2.11, replace `atPhase(currentRun.typerPhase.next)` by `exitingPhase(currentRun.typerPhase)`
      val typedArgs = atPhase(currentRun.typerPhase.next){ args map (typer.typed(_)) }
      List(AnnotationInfo(relClass.tpe, typedArgs, List()))
    }
  }

  def joinRel(r1: List[RelEffect], r2: List[RelEffect]): List[RelEffect] = {
    var res = r1
    for (e2 <- r2) {
      // include e2 into res. First find relative effects in res that have the same param
      val (sameParam, others) = res.partition(e1 => e1.param == e2.param)
      if (sameParam.isEmpty) {
        res = e2 :: others
      } else {
        res = sameParam.find(_.fun.isEmpty) match {
          case Some(e) =>
            // if there's an existing relative effect which covers all methods, take only that.
            e :: others
          case None =>
            // if e2 covers all methods, take only e2, drop the existing ones with the same param
            if (e2.fun.isEmpty) e2 :: others
            // otherwise, if the same relative effect already exists, drop e2
            else if (sameParam.exists(_.fun == e2.fun)) res
            // otherwise, include e2
            else e2 :: res
        }
      }
    }
    res
  }

  def joinAllRel(rels: List[RelEffect]*) = {
    if (rels.isEmpty) Nil
    else (rels.tail :\ rels.head)(joinRel)
  }

  def meetRel(r1: List[RelEffect], r2: List[RelEffect]): List[RelEffect] = {
    var res = List[RelEffect]()
    for (e1 <- r1; e2 <- r2) {
      // TODO: probably wrong; might produce duplicates in res.
      if      (e1 <= e2) res = e1 :: res
      else if (e2 <= e1) res = e2 :: res
    }
    res
  }

  def meetAllRel(rels: List[RelEffect]*) = {
    if (rels.isEmpty) Nil
    else (rels.tail :\ rels.head)(meetRel)
  }

  def lteRel(r1: List[RelEffect], r2: List[RelEffect]): Boolean = {
    r1.forall(lteRelOne(_, r2))
  }

  def lteRelOne(r1: RelEffect, r2: List[RelEffect]): Boolean = {
    r2.exists(e2 => r1 <= e2)
  }


  case class RelEffect(param: Loc, fun: Option[Symbol]) {
    def <= (other: RelEffect) =
      this.param == other.param && (this.fun == other.fun || other.fun.isEmpty)
  }

  trait Loc
  case class ThisLoc(cls: Symbol) extends Loc
  case class ParamLoc(param: Symbol) extends Loc {
    override def hashCode() = param.hashCode()
    override def equals(other: Any) = other match {
      case ParamLoc(otherParam) => sameParam(param, otherParam)
      case _ => false
    }
  }

  /**
   * True if `a` and `b` denote the same parameter.
   *
   * We cannot just compare the symbols for equality. The reason is that there might be
   * multiple symbols for the same parameter, the one in the MethodType can be different
   * than the one assigned to trees. (When cloning a MethodType, new parameter symbols
   * get created). Therefore, we commpare owner (the method) and name.
   */
  def sameParam(a: Symbol, b: Symbol): Boolean =
    a.owner == b.owner && a.name == b.name
}
