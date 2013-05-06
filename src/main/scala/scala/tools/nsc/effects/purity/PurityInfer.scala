package scala.tools.nsc.effects
package purity

// `extends Infer` allows overriding methods in Infer
trait PurityInfer extends Infer { this: PurityDomain =>
  import global._
  import lattice._

  override def explainMismatch(expected: Effect, found: Effect): Option[String] = {
    import PurityEffect._
    def msg(a: List[String], b: List[String]) = s"${a.mkString(" ")} does not conform to ${b.mkString(" ")}"
    val mod = {
      if (found.mod lte expected.mod) Nil
      else List(msg(modToString(found.mod), modToString(expected.mod)))
    }
    val assign = {
      if (found.assign lte expected.assign) Nil
      else List(msg(assignToString(found.assign, showEmpty = true), assignToString(expected.assign, showEmpty = true)))
    }
    val loc = {
      if (found.loc lte expected.loc) Nil
      else List(msg(resLocToString(found.loc), resLocToString(expected.loc)))
    }
    val msgs = (mod ::: assign ::: loc)
    if (msgs.isEmpty) None
    else Some(msgs.mkString("; "))
  }

  
  override def computeEffectImpl(tree: Tree, ctx: EffectContext): Effect = {
    lazy val sym = tree.symbol
    tree match {

      // @TODO: no more needed?
      case ValDef(_, _, _, rhs) /*if sym.isVariable*/ =>
        val PurityEffect(rhsMod, rhsAssign, rhsLoc) = computeEffect(rhs, ctx)
        // use an @assing effect for the initial assignment, also for non-variables. when
        // the variable gets out of scope the assign effect will be taken into account.
        val assignEff = Assigns((sym, rhsLoc))
        PurityEffect(rhsMod, rhsAssign join assignEff, AnyLoc)

      case Ident(name) if !sym.isMethod =>
        if (sym == NoSymbol) {
          assert(ctx.patternMode && name == nme.WILDCARD)
          lattice.noModAnyResLoc
        } else if (sym.isValueParameter || sym.isLocal)
          PurityEffect(RefSet(), Assigns(), RefSet(SymRef(sym)))
        else
          lattice.noModAnyResLoc

      case This(name) =>
        PurityEffect(RefSet(), Assigns(), RefSet(ThisRef(sym)))


      // assignments to fields - note that variable fields are usually modified using the setter
      case Assign(sel @ Select(qual, _), rhs) =>
        val fieldSym = sel.symbol
        assert(fieldSym.isVariable && !fieldSym.isLocal, s"expected variable field, found $fieldSym")
        val PurityEffect(qualMod, qualAssign, qualLoc) = computeEffect(qual, ctx)
        val PurityEffect(rhsMod, rhsAssign, rhsLoc) = computeEffect(rhs, ctx)
        val assignMod = if (isLocalField(fieldSym)) qualLoc join rhsLoc
                        else qualLoc
        // result locality AnyLoc: assignments evaluate to the unit value ()
        PurityEffect(qualMod join rhsMod join assignMod, qualAssign join rhsAssign, AnyLoc)


      case Assign(id @ Ident(name), rhs) =>
        val varSym = id.symbol
        assert(varSym.isVariable && varSym.isLocal, s"expected local variable, found $sym")
        val PurityEffect(rhsEff, rhsAssign, rhsLoc) = computeEffect(rhs, ctx)
        val assignEff = Assigns((varSym, rhsLoc))
        PurityEffect(rhsEff, rhsAssign join assignEff, AnyLoc)


      // direct field access - note that most field accesses go through getters
      case Select(qual, name) if !sym.isMethod =>
        val PurityEffect(qualMod, qualAssign, qualLoc) = computeEffect(qual, ctx)
        val resloc = if (isLocalField(sym)) qualLoc else AnyLoc
        PurityEffect(qualMod, qualAssign, resloc)


      case Block(stats, expr) =>
        // allow effects to local variables the block

        val locals = stats collect {
          case vd: ValDef => vd.symbol
        }

        val (statsExpected, exprExpected) = ctx.expected match {
          case None =>
            (None, None)

          case Some(PurityEffect(mod, assign, loc)) =>
            val localsLocality = RefSet(locals.map(SymRef).toSet[VarRef])
            val exMod = mod join localsLocality
            val exAssign = assign join Assigns(locals.map(sym => (sym, AnyLoc)).toMap)
            val exLoc = loc join localsLocality
            (Some(PurityEffect(exMod, exAssign, AnyLoc)), Some(PurityEffect(exMod, exAssign, exLoc)))
        }

        val (statsMods, statsAssigns, _) = stats.map(computeEffect(_, ctx.copy(expected = statsExpected)).toTriple).unzip3
        val PurityEffect(exprMod, exprAssign, exprLoc) = computeEffect(expr, ctx.copy(expected = exprExpected))

        val allMod = joinAllLocalities(statsMods, exprMod)
        val allAssign = joinAllAssignEffs(statsAssigns, exprAssign)

        
        // eliminate assignment effects to local variables, substitute their locality in the resulting effect
        allAssign match {
          case AssignAny =>
            // the block has unknown assignment effects, in this case we don't know the overall effect of the
            // block because we don't know how variables might be aliased
            PurityEffect(AnyLoc, AssignAny, AnyLoc)

          case Assigns(as) =>
            // the block has some assignment effects. for assignments to local variables allocated in this block,
            // the assignment effect expresses the locality that the variable might point to. example
            //   var x = a
            //   x = b
            // has effect x->(a,b). in the resulting effect we need to eliminate all references to the local
            // variables that get out of scope, we replace them by their locality.
            //
            // however, the locality of a local variable might include other local variables allocated in the block:
            //   var x = a   // assignment effect x->(a)
            //   var y = x   // assignment effect y->(x)
            //   y.modify()  // mod effect y
            // here we have assignment effect y->(x), but in the modification effect we should replace (y) by (a),
            // not by (x). so we compute `substMap`, a substitution map from local variables to localities which
            // don't refer to any locals of this block. in the example, substMap would be x->(a),y->(a).
            val (localAssigns, otherAssigns) = as.partition(p => locals.contains(p._1))
            val elimSelf: Map[VarRef, Locality] = localAssigns.map(p => (SymRef(p._1), elimSym(p._1, p._2)))
            val substMap = (elimSelf /: elimSelf) {
              case (substMap, (ref, locality)) =>
                substMap.map(p => (p._1, substitute(Map(ref -> locality), p._2)))
            }
            substitute(substMap, PurityEffect(allMod, Assigns(otherAssigns), exprLoc))
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
        val PurityEffect(condMod, condAssign, _) = computeEffect(cond, contextWithAnyLocExpected(ctx))
        val PurityEffect(thenMod, thenAssign, thenLoc) = computeEffect(thenp, ctx)
        val PurityEffect(elseMod, elseAssign, elseLoc) = computeEffect(elsep, ctx)
        PurityEffect(
          condMod join thenMod join elseMod,
          condAssign join thenAssign join elseAssign,
          thenLoc join elseLoc)

      case Match(sel, cases) =>
        val PurityEffect(selMod, selAssign, _) = computeEffect(sel, contextWithAnyLocExpected(ctx))
        val (casesMods, casesAssigns, casesLocs) = cases.map(computeEffect(_, ctx).toTriple).unzip3
        PurityEffect(
          joinAllLocalities(casesMods, selMod),
          joinAllAssignEffs(casesAssigns, selAssign),
          joinAllLocalities(casesLocs))

      case CaseDef(pat, guard, rhs) =>
        val anyLocCtx = contextWithAnyLocExpected(ctx)
        val PurityEffect(pagMod, patAssign, _) = computeEffect(pat, anyLocCtx.copy(patternMode = true))
        val PurityEffect(guardMod, guardAssign, _) = computeEffect(guard, anyLocCtx)
        val PurityEffect(rhsMod, rhsAssign, rhsLoc) = computeEffect(rhs, ctx)
        PurityEffect(
          joinAllLocalities(List(pagMod, guardMod), rhsMod),
          joinAllAssignEffs(List(patAssign, guardAssign), rhsAssign),
          rhsLoc)


      case Try(block, catches, finalizer) =>
        val PurityEffect(blockMod, blockAssign, blockLoc) = computeEffect(block, ctx)
        val (catchesMods, catchesAssigns, catchesLocs) = catches.map(computeEffect(_, ctx).toTriple).unzip3
        val PurityEffect(finalizerMod, finalizerAssign, _) = computeEffect(finalizer, contextWithAnyLocExpected(ctx))
        PurityEffect(
          joinAllLocalities(blockMod :: catchesMods, finalizerMod),
          joinAllAssignEffs(blockAssign :: catchesAssigns, finalizerAssign),
          joinAllLocalities(catchesLocs, blockLoc))

          
      case _ =>
        super.computeEffectImpl(tree, ctx)
    }
  }

  def contextWithAnyLocExpected(ctx: EffectContext) = {
    val exp = ctx.expected map {
      case PurityEffect(exMod, exAssign, _) => PurityEffect(exMod, exAssign, AnyLoc)
    }
    ctx.copy(expected = exp)
  }

  /**
   * Eliminate `SymRef(sym)` references from the locality `loc`.
   */
  def elimSym(sym: Symbol, loc: Locality): Locality = loc match {
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
  override def adaptApplyEffect(fun: Symbol, funEff: Effect, byValEffs: Map[Symbol, Effect],
                                repeatedEffs: Map[Symbol, List[Effect]], latent: Effect): Effect = {

    val PurityEffect(resMod, resAssign, _) = super.adaptApplyEffect(fun, funEff, byValEffs, repeatedEffs, latent)
    val argLocs: Map[VarRef, Locality] = byValEffs map {
      case (sym, eff) => (SymRef(sym), eff.loc)
    }
    val substMap = {
      if (fun.isLocal) argLocs // for local functions, `this` is not substituted
      else argLocs + ((ThisRef(fun.owner), funEff.loc))
    }

    val PurityEffect(_, _, resLoc) = fromAnnotation(fun.info)

    substitute(substMap, PurityEffect(resMod, resAssign, resLoc))
  }

  def isLocalField(sym: Symbol) =
    sym.hasAnnotation(localClass)

  def substitute(map: Map[VarRef, Locality], e: Effect): Effect =
    PurityEffect(substitute(map, e.mod), substitute(map, e.assign), substitute(map, e.loc))

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

  /**
   * Set the result locality of constructors to be fresh. This method is called when the effect
   * of a primary constructor is inferred, to make sure the constructor returns a fresh object.
   */
  override def adaptInferredPrimaryConstrEffect(
      constrDef: DefDef,
      rhsEff: Effect,
      fieldEffs: Map[Symbol, Effect],
      statEffs: List[Effect],
      traitParentConstrEffs: Map[Symbol, Effect]): Effect = {
    val combinedEff = super.adaptInferredPrimaryConstrEffect(constrDef, rhsEff, fieldEffs, statEffs, traitParentConstrEffs)
    
    val fieldInitEffs = fieldEffs.toList map {
      case (fieldSym, eff) =>
        val mod = {
          val modThis = RefSet(ThisRef(fieldSym.owner))
          if (isLocalField(fieldSym)) eff.loc join modThis
          else modThis
        }
        PurityEffect(mod, Assigns(), AnyLoc)
    }
    
    val storeLocalParamsEff = {
      val params = constrDef.vparamss.flatten.map(_.symbol)
      if (params.isEmpty) bottom
      else {
        val thisRef = ThisRef(constrDef.symbol.owner)
        val localParamRefs: Set[VarRef] = params.filter(isLocalField).map(p => SymRef(p)).toSet
        PurityEffect(RefSet(localParamRefs + thisRef), Assigns(), AnyLoc)
      }
    }
    
    ((combinedEff /: fieldInitEffs)(_ u _) u storeLocalParamsEff).copy(loc = RefSet())
  }
  
  override def adaptInferredMethodEffect(method: Symbol, eff: Effect) = {
    if (method.isConstructor) eff.copy(loc = RefSet())
    else eff
  }

  /**
   * This is a dual of the above: when computing the effect of a constructor, change the expected
   * effect and allow `@loc(any)` as result locality. This hook is for constructors that are
   * annotated as fresh, it is used when the actual effect of the constructor is checked.
   */
  override def adaptExpectedMethodEffect(method: Symbol, eff: Effect) = {
    if (method.isConstructor) eff.copy(loc = AnyLoc)
    else eff    
  }
}
