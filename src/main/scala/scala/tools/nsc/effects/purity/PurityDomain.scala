package scala.tools.nsc.effects
package purity

abstract class PurityDomain extends EffectDomain with ConvertAnnots with PurityInfer {
  import global._

  override val requireANF: Boolean = true

  lazy val lattice = new PurityLattice {
    val global: PurityDomain.this.global.type = PurityDomain.this.global
  }
  import lattice._

  override def accessorEffect(sym: Symbol, tpe: Type, tree: Tree): Effect = {
    if (sym.isGetter) getterEffect(sym)
    else setterEffect(sym, tpe)
  }

  def getterEffect(sym: Symbol): Effect = {
    val owner = sym.owner
    val ref = {
      if (owner.isModuleClass) SymRef(owner.sourceModule)
      else ThisRef(owner)
    }
    // for abstract fields, there is no field symbol. so we check the annotation on the getter.
    // used to be   (atPhase(currentRun.typerPhase)(sym.hasAnnotation(localClass)))
    if (sym.hasAnnotation(localClass)) {
      (RefSet(), Assigns(), RefSet(ref))
    } else {
      lattice.noModAnyResLoc
    }
  }

  def setterEffect(sym: Symbol, tpe: Type): Effect = {
    val owner = sym.owner
    if (owner.isModuleClass) {
      lattice.top
    } else {
      val ref: VarRef = ThisRef(owner)
      // for abstract fields, there is no field symbol. so we check the annotation on the getter.
      // used to be   (atPhase(currentRun.typerPhase)(getter.hasAnnotation(localClass)))
      val getter = sym.getter(sym.owner)
      if (getter.hasAnnotation(localClass)) {
        // luckily we have the type tpe here - calling sym.info would lead to a cyclic reference
        val List(List(arg)) = tpe.paramss
        (RefSet(Set(ref, SymRef(arg))), Assigns(), AnyLoc)
      } else {
        (RefSet(ref), Assigns(), AnyLoc)
      }
    }
  }
}
