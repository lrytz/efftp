package scala.tools.nsc.effects.purity

trait PurityInfer { this: PurityDomain =>
  import global._
  import lattice._

  override def computeEffectImpl(tree: Tree, ctx: EffectContext): Effect = {
    lazy val sym = tree.symbol
    tree match {

      case ValDef(_, _, _, rhs) if sym.isVariable =>
        val (rhsEff, rhsLoc) = computeEffect(rhs, ctx)
        sym.updateAttachment(LocalVarInitialLocality(rhsLoc))
        (rhsEff, rhsLoc)

      case Ident(_) =>
        if (sym.isValueParameter || sym.isLocal)
          (RefSet(), RefSet(SymRef(sym)))
        else
          lattice.noModAnyResLoc

      case This(name) =>
        (RefSet(), RefSet(ThisRef(sym)))


      // assignments to fields - note that variable fields are usually modified using the setter
      case Assign(Select(qual, _), rhs) =>
        assert(sym.isVariable && !sym.isLocal, s"expected variable field, found $sym")
        val (qualEff, qualLoc) = computeEffect(qual, ctx)
        val (rhsEff, rhsLoc) = computeEffect(rhs, ctx)
        val assignEff = if (isLocalField(sym)) qualLoc union rhsLoc
                        else qualLoc
        // result locality AnyLoc: assignments evaluate to the unit value ()
        (qualEff union rhsEff union assignEff, AnyLoc)


      case Assign(Ident(name), rhs) =>
        assert(sym.isVariable && sym.isLocal, s"expected local variable, found $sym")
        val initLoc = initialLocality(sym)
        val (rhsEff, rhsLoc) = computeEffect(rhs, ctx)
        // @TODO: for now we just use the top effect when the assigned locality is larger than the initial one.
        // probably sound - future statements that effect on the variable are not typed as pure, they are typed
        // as modifying that variable. the entire block where the variable is defined will not type as pure.
        if (rhsLoc subsetOf initLoc) lattice.noModAnyResLoc
        else lattice.top



      // direct field access - note that most field accesses go through getters
      case Select(qual, name) if !sym.isMethod =>
        val (mod, loc) = computeEffect(qual, ctx)
        val resloc = if (isLocalField(sym)) loc else AnyLoc
        (mod, resloc)

      case _ =>
        lattice.noModAnyResLoc

    }
  }

  private case class LocalVarInitialLocality(loc: Locality)

  def isLocalField(sym: Symbol) =
    sym.hasAnnotation(localClass)

  /**
   * TODO: problem if the variable is defined in an outer method whose effect has not been computed yet.
   * then the attachment is missing:
   *
   *   def f() {
   *     var a = new A
   *     def bar() { a = someGlobalA }
   *   }
   *
   * if the effect of bar() is computed before the effect of f() then the symbol for a will not have an
   * attachment.
   *
   *   ==> need @assign effect for nested methods
   */
  def initialLocality(localVar: Symbol): Locality = {
    val att = localVar.attachments.get[LocalVarInitialLocality]
    att.map(_.loc).getOrElse(RefSet())
  }
}
