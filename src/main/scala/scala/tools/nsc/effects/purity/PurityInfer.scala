package scala.tools.nsc.effects.purity

trait PurityInfer { this: PurityDomain =>
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


//      case Block

      case _ =>
        lattice.noModAnyResLoc
    }
  }

  def isLocalField(sym: Symbol) =
    sym.hasAnnotation(localClass)
}
