package scala.tools.nsc.effects
package state

abstract class StateDomain extends EffectDomain with ConvertAnnots with StateInfer {

  import global._

  lazy val lattice = new StateLattice {
    val global: StateDomain.this.global.type = StateDomain.this.global
  }
  import lattice._


  override def accessorEffect(sym: Symbol): Effect = {
    if (sym.isGetter) getterEffect(sym)
    else setterEffect(sym)
  }


  def getterEffect(sym: Symbol): Effect = {
    val owner = sym.owner
    val loc = {
      if (owner.isModuleClass) SymLocation(owner.sourceModule)
      else ThisLocation(owner)
    }
    // for abstract fields, there is no field symbol. so we check the annotation on the getter.
    // used to be   (atPhase(currentRun.typerPhase)(sym.hasAnnotation(localClass)))
    if (sym.hasAnnotation(localClass)) {
      (StoreLoc(), LocSet(loc))
    } else {
      lattice.noModAnyLoc
    }
  }

  def setterEffect(sym: Symbol): Effect = {
    val owner = sym.owner
    if (owner.isModuleClass) {
      lattice.top
    } else {
      val loc = ThisLocation(owner)
      // for abstract fields, there is no field symbol. so we check the annotation on the getter.
      // used to be   (atPhase(currentRun.typerPhase)(getter.hasAnnotation(localClass)))
      val getter = sym.getter(sym.owner)
      if (getter.hasAnnotation(localClass)) {
        val List(List(arg)) = sym.paramss
        (StoreLoc(loc, LocSet(SymLocation(arg))), AnyLoc)
      } else {
        (StoreLoc(loc, LocSet(Fresh)), AnyLoc)
      }
    }
  }
}
