package scala.tools.nsc.effects

trait Infer { self: EffectDomain =>
  import global._
  import lattice._

  /**
   * Compute the effect of some tree. Needs enclFun for effect polymorphism
   */
  final def inferEffect(tree: Tree, enclFun: Symbol): Effect = {
    val infer = new EffectInfer(enclFun)
    infer.traverse(tree)
    infer.effect
  }

  private class EffectInfer(enclFun: Symbol) extends Traverser {
    var effect: Effect = bottom

    override def traverse(tree: Tree) {
      computeEffect(tree, enclFun, e => {effect = effect u e}, super.traverse(tree))
    }
  }

  /**
   * TODO: rename
   *
   * compute the effect of a tree. called by the EffectInfer traverser on every sub-tree.
   * overriding this method allows concrete domains to specify the effect of
   * certain trees.
   *
   * TODO: continue could be "traverse: Tree => Unit", or even "traverser: Traverser"
   * for more generality / to give more possibilities to overriding domains
   */
  def computeEffect(tree: Tree, enclFun: Symbol, set: Effect => Unit, continue: => Unit) {
    tree match {
      case Apply(_, _) =>
        computeApplyEffect(tree, enclFun, set, continue)

      case TypeApply(_, _) =>
        computeApplyEffect(tree, enclFun, set, continue)

      case Select(qual, _) =>
        if (tree.symbol.isMethod)
          computeApplyEffect(tree, enclFun, set, continue)
        else
          continue

      case Ident(_) =>
        if (tree.symbol.isMethod) {
          // parameterless local methods are applied using an `Ident` tree
          computeApplyEffect(tree, enclFun, set, continue)
        }
      case _ =>
        continue
    }
  }

  def computeApplyEffect(tree: Tree, enclFun: Symbol, set: Effect => Unit, continue: => Unit) {
    val (fun, _, argss) = decomposeApply(tree)
    val args = argss.flatten

    val sym = fun.symbol
    val funEff   = fun match {
      case Select(qual, _) => inferEffect(qual, enclFun)
      case Ident(_) => bottom
    }
    val argsEffs = args map (inferEffect(_, enclFun))

    val relEnv = relEffects(enclFun)

    val lat = {
      if (hasRelativeEffect(fun, relEnv)) bottom
      else {
        val paramLocs = sym.paramss.flatten.map(ParamLoc)
        // @TODO: should probably add ThisLoc to the map. can the type of this ever be more specific?
        latent(sym, paramLocs.zip(args.map(argTpeAndLoc)).toMap, relEnv)
      }
    }
    val e = funEff u (argsEffs :\ bottom)(_ u _) u lat
    set(e)
  }

  private val argTpeAndLoc: Tree => (Type, Option[Loc]) = arg => {
    val tp = arg.tpe
    arg match {
      case Ident(_) if arg.symbol.owner.isMethod =>
        (tp, Some(ParamLoc(arg.symbol)))
      case This(_) =>
        (tp, Some(ThisLoc(arg.symbol)))
      case _ =>
        (tp, None)
    }
  }

  private def decomposeApply(tree: Tree): (Tree, List[Tree], List[List[Tree]]) = {
    var baseFun: Tree = null
    var targs: List[Tree] = Nil
    def getArgs(t: Tree): List[List[Tree]] = t match {
      case Apply(fun, args) =>
        args :: getArgs(fun)
      case TypeApply(fun, targs0) =>
        targs = targs0
        getArgs(fun)
      case _ =>
        baseFun = t
        Nil
    }
    val argss = getArgs(tree)
    (baseFun, targs, argss.reverse)
  }

  private def hasRelativeEffect(fun: Tree, relEnv: List[RelEffect]): Boolean = {
    val sym = fun.symbol
    fun match {
      case Select(id @ Ident(_), _) =>
        val r = RelEffect(ParamLoc(id.symbol), Some(sym))
        lteRel(List(r), relEnv)

      case Select(th @ This(_), _) =>
        val r = RelEffect(ThisLoc(th.symbol), Some(sym))
        lteRel(List(r), relEnv)

      case _ => false
    }
  }


  // @TODO: fixpoint computation needed (rel effects can go in circles!)?
  // @TODO: document what happens here (in general, argtps map specifically)
  private def latent(fun: Symbol, argtps: Map[Loc, (Type, Option[Loc])], relEnv: List[RelEffect]): Effect = {
    val concrete = fromAnnotation(fun.info)
    val relEff = relEffects(fun)

    val expandedRelEff = relEff map { r =>
      if (lteRelOne(r, relEnv)) bottom
      else r match {
        case RelEffect(paramLoc, Some(fun)) if (argtps contains paramLoc) =>
          argtps(paramLoc) match {
            case (tp, Some(argLoc)) if lteRelOne(RelEffect(argLoc, Some(fun)), relEnv) =>
              // @TODO: document. if a parameter is forwarded to another method as parameter, detect rel effects
              bottom
            case (tp, _) =>
              val funSym = tp.member(fun.name).suchThat(m => m.overriddenSymbol(fun.owner) == fun || m == fun)
              latent(funSym, Map(), relEnv)
          }

        case RelEffect(_, Some(fun)) =>
          latent(fun, Map(), relEnv)

        case _ =>
          // todo: union of effects of all methods
          top
      }
    }
    concrete u (expandedRelEff :\ bottom)(_ u _)
  }

}
