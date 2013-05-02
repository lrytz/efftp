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

      case Ident(name) if !sym.isMethod =>
        if (sym == NoSymbol) {
          assert(ctx.patternMode && name == nme.WILDCARD)
          lattice.noModAnyResLoc
        } else if (sym.isValueParameter || sym.isLocal)
          (RefSet(), Assigns(), RefSet(SymRef(sym)))
        else
          lattice.noModAnyResLoc

      case This(name) =>
        (RefSet(), Assigns(), RefSet(ThisRef(sym)))


      // assignments to fields - note that variable fields are usually modified using the setter
      case Assign(sel @ Select(qual, _), rhs) =>
        val fieldSym = sel.symbol
        assert(fieldSym.isVariable && !fieldSym.isLocal, s"expected variable field, found $fieldSym")
        val (qualMod, qualAssign, qualLoc) = computeEffect(qual, ctx)
        val (rhsMod, rhsAssign, rhsLoc) = computeEffect(rhs, ctx)
        val assignMod = if (isLocalField(fieldSym)) qualLoc join rhsLoc
                        else qualLoc
        // result locality AnyLoc: assignments evaluate to the unit value ()
        (qualMod join rhsMod join assignMod, qualAssign join rhsAssign, AnyLoc)


      case Assign(id @ Ident(name), rhs) =>
        val varSym = id.symbol
        assert(varSym.isVariable && varSym.isLocal, s"expected local variable, found $sym")
        val (rhsEff, rhsAssign, rhsLoc) = computeEffect(rhs, ctx)
        val assignEff = Assigns((varSym, rhsLoc))
        (rhsEff, rhsAssign join assignEff, AnyLoc)


      // direct field access - note that most field accesses go through getters
      case Select(qual, name) if !sym.isMethod =>
        val (qualMod, qualAssign, qualLoc) = computeEffect(qual, ctx)
        val resloc = if (isLocalField(sym)) qualLoc else AnyLoc
        (qualMod, qualAssign, resloc)


      case Block(stats, expr) =>
        // allow effects to local variables the block

        val locals = stats collect {
          case vd: ValDef => vd.symbol
        }

        val (statsExpected, exprExpected) = ctx.expected match {
          case None =>
            (None, None)

          case Some((mod, assign, loc)) =>
            val localsLocality = RefSet(locals.map(SymRef).toSet[VarRef])
            val exMod = mod join localsLocality
            val exAssign = assign join Assigns(locals.map(sym => (sym, AnyLoc)).toMap)
            val exLoc = loc join localsLocality
            (Some((exMod, exAssign, AnyLoc)), Some((exMod, exAssign, exLoc)))
        }

        val (statsMods, statsAssigns, _) = stats.map(computeEffect(_, ctx.copy(expected = statsExpected))).unzip3
        val (exprMod, exprAssign, exprLoc) = computeEffect(expr, ctx.copy(expected = exprExpected))

        val allMod = joinAllLocalities(statsMods, exprMod)
        val allAssign = joinAllAssignEffs(statsAssigns, exprAssign)

        allAssign match {
          case AssignAny =>
            (AnyLoc, AssignAny, AnyLoc)

          case Assigns(as) =>
            val (localAssigns, otherAssigns) = as.partition(p => locals.contains(p._1))
            val substMap: Map[VarRef, Locality] = localAssigns.map(p => (SymRef(p._1), elimSym(p._1, p._2)))
            substitute(substMap, (allMod, Assigns(otherAssigns), exprLoc))
        }

      case New(tpt) =>
        bottom // pure and fresh

      case Literal(c) =>
        effectForPureAnnotated // pure but not fresh


      /* For method calls, set the expected locality to `Any`. The reason is that the effects for the function
       * tree and for the arguments are computed using the expected locality that we use here. However the
       * current expected locality only applies for the resulting value, and it is checked once we return from
       * here.
       *
       * Note that at this point, Ident and Select trees can only be (parameterless) method calls, the other
       * case is treated in cases above
       */
      case _: Apply | _: TypeApply | _: Ident | _: Select | _: UnApply =>
        super.computeEffectImpl(tree, contextWithAnyLocExpected(ctx))


      case If(cond, thenp, elsep) =>
        val (condMod, condAssign, _) = computeEffect(cond, contextWithAnyLocExpected(ctx))
        val (thenMod, thenAssign, thenLoc) = computeEffect(thenp, ctx)
        val (elseMod, elseAssign, elseLoc) = computeEffect(elsep, ctx)
        (
          condMod join thenMod join elseMod,
          condAssign join thenAssign join elseAssign,
          thenLoc join elseLoc)

      case Match(sel, cases) =>
        val (selMod, selAssign, _) = computeEffect(sel, contextWithAnyLocExpected(ctx))
        val (casesMods, casesAssigns, casesLocs) = cases.map(computeEffect(_, ctx)).unzip3
        (
          joinAllLocalities(casesMods, selMod),
          joinAllAssignEffs(casesAssigns, selAssign),
          joinAllLocalities(casesLocs))

      case CaseDef(pat, guard, rhs) =>
        val anyLocCtx = contextWithAnyLocExpected(ctx)
        val (pagMod, patAssign, _) = computeEffect(pat, anyLocCtx.copy(patternMode = true))
        val (guardMod, guardAssign, _) = computeEffect(guard, anyLocCtx)
        val (rhsMod, rhsAssign, rhsLoc) = computeEffect(rhs, ctx)
        (
          joinAllLocalities(List(pagMod, guardMod), rhsMod),
          joinAllAssignEffs(List(patAssign, guardAssign), rhsAssign),
          rhsLoc)


      case Try(block, catches, finalizer) =>
        val (blockMod, blockAssign, blockLoc) = computeEffect(block, ctx)
        val (catchesMods, catchesAssigns, catchesLocs) = catches.map(computeEffect(_, ctx)).unzip3
        val (finalizerMod, finalizerAssign, _) = computeEffect(finalizer, contextWithAnyLocExpected(ctx))
        (
          joinAllLocalities(blockMod :: catchesMods, finalizerMod),
          joinAllAssignEffs(blockAssign :: catchesAssigns, finalizerAssign),
          joinAllLocalities(catchesLocs, blockLoc))


      case _ =>
        super.computeEffectImpl(tree, ctx)
    }
  }

  def contextWithAnyLocExpected(ctx: EffectContext) = {
    val exp = ctx.expected map {
      case (exMod, exAssign, _) => (exMod, exAssign, AnyLoc)
    }
    ctx.copy(expected = exp)
  }

  /**
   * Eliminate `SymRef(sym)` references from the locality `loc`.
   */
  def elimSym(sym: Symbol, loc: Locality) = loc match {
    case AnyLoc => AnyLoc
    case RefSet(refs) => RefSet(refs.filter {
      case SymRef(s) if s == sym => false
      case _ => true
    })
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
