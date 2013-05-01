package scala.tools.nsc.effects
package purity

trait PurityInfer extends Infer { this: PurityDomain =>
  import global._
  import lattice._

  override def computeEffectImpl(tree: Tree, ctx: EffectContext): Effect = {
    lazy val sym = tree.symbol
    tree match {

      // @TODO: no more needed?
      case ValDef(_, _, _, rhs) /*if sym.isVariable*/ =>
        val (rhsMod, rhsAssign, rhsLoc) = computeEffect(rhs, ctx)
        // use an @assing effect for the initial assignment, also for non-variables. when
        // the variable gets out of scope the assign effect will be taken into account.
        val assignEff = Assigns((sym, rhsLoc))
        (rhsMod, rhsAssign join assignEff, AnyLoc)

      case Ident(_) =>
        if (sym.isValueParameter || sym.isLocal)
          (RefSet(), Assigns(), RefSet(SymRef(sym)))
        else
          lattice.noModAnyResLoc

      case This(name) =>
        (RefSet(), Assigns(), RefSet(ThisRef(sym)))


      // assignments to fields - note that variable fields are usually modified using the setter
      case Assign(Select(qual, _), rhs) =>
        assert(sym.isVariable && !sym.isLocal, s"expected variable field, found $sym")
        val (qualMod, qualAssign, qualLoc) = computeEffect(qual, ctx)
        val (rhsMod, rhsAssign, rhsLoc) = computeEffect(rhs, ctx)
        val assignMod = if (isLocalField(sym)) qualLoc join rhsLoc
                        else qualLoc
        // result locality AnyLoc: assignments evaluate to the unit value ()
        (qualMod join rhsMod join assignMod, qualAssign join rhsAssign, AnyLoc)


      case Assign(Ident(name), rhs) =>
        assert(sym.isVariable && sym.isLocal, s"expected local variable, found $sym")
        val (rhsEff, rhsAssign, rhsLoc) = computeEffect(rhs, ctx)
        val assignEff = Assigns((sym, rhsLoc))
        (rhsEff, rhsAssign join assignEff, AnyLoc)


      // direct field access - note that most field accesses go through getters
      case Select(qual, name) if !sym.isMethod =>
        val (qualMod, qualAssign, qualLoc) = computeEffect(qual, ctx)
        val resloc = if (isLocalField(sym)) qualLoc else AnyLoc
        (qualMod, qualAssign, resloc)


      case Block(stats, expr) =>
        val (statsMods, statsAssigns, _) = stats.map(computeEffect(_, ctx)).unzip3
        val (exprMod, exprAssign, exprLoc) = computeEffect(expr, ctx)

        val allMod = joinAllLocalities(statsMods, exprMod)
        val allAssign = joinAllAssignEffs(statsAssigns, exprAssign)

        val locals = stats collect {
          case vd: ValDef => vd.symbol
        }

        allAssign match {
          case AssignAny =>
            (AnyLoc, AssignAny, AnyLoc)

          case Assigns(as) =>
            val (localAssigns, otherAssigns) = as.partition(p => locals.contains(p._1))
            val substMap: Map[VarRef, Locality] = localAssigns.map(p => (SymRef(p._1), p._2))
            substitute(substMap, (allMod, Assigns(otherAssigns), exprLoc))
        }


      case New(tpt) =>
        bottom // pure and fresh

      case _ =>
        super.computeEffectImpl(tree, ctx)
    }
  }

  /**
   * For method invocations need to
   *
   *  - substitute references to parameters by the argument localities in the resulting effect
   *
   *  - fix the result locality: super.computeApplyEffect joins all the involved localities, but the
   *    real result locality is what is annotated on the function type. Effects and result localities
   *    propagate differently.
   */
  override def combineApplyEffect(fun: Symbol, funEff: Effect, byValEffs: Map[Symbol, Effect],
                                  repeatedEffs: Map[Symbol, List[Effect]], latent: Effect): Effect = {

    val (resMod, resAssign, _) = super.combineApplyEffect(fun, funEff, byValEffs, repeatedEffs, latent)
    val argLocs: Map[VarRef, Locality] = byValEffs map {
      case (sym, eff) => (SymRef(sym), eff._3)
    }
    val substMap = {
      if (fun.isLocal) argLocs // for local functions, `this` is not substituted
      else argLocs + ((ThisRef(fun.owner), funEff._3))
    }

    val (_, _, resLoc) = fromAnnotation(fun.info)

    substitute(substMap, (resMod, resAssign, resLoc))
  }

  def isLocalField(sym: Symbol) =
    sym.hasAnnotation(localClass)

  def substitute(map: Map[VarRef, Locality], e: Effect): Effect =
    (substitute(map, e._1), substitute(map, e._2), substitute(map, e._3))

  def substitute(map: Map[VarRef, Locality], loc: Locality): Locality = loc match {
    case AnyLoc =>
      AnyLoc

    case RefSet(refs) =>
      val locs = refs.toList.map(ref => map.getOrElse(ref, RefSet(ref)))
      joinAllLocalities(locs)
  }

  def substitute(map: Map[VarRef, Locality], assignEff: AssignEff): AssignEff = assignEff match {
    case AssignAny =>
      AssignAny

    case Assigns(as) =>
      Assigns(as map {
        case (sym, loc) => (sym, substitute(map, loc))
      })
  }
}
