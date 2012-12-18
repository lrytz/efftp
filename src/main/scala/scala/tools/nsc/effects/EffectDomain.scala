package scala.tools.nsc.effects

import scala.tools.nsc._

abstract class EffectDomain extends Infer with RelEffects {
  // `global` should not be a class parameter. Having it a field allows to refine
  // its type, e.g. `EffectDomain { val global: some.global.type }`, which is not
  // possible for parameters.
  val global: Global

  val lattice: EffectLattice
  import lattice.Effect

  import global._

  val annotationClasses: List[Symbol]

  /**
   * Read the effect from a list of annotations. If no effect annotation for this domain
   * can be found, this method should return the `default` effect.
   */
  def fromAnnotation(annots: List[AnnotationInfo], default: => Effect): Effect
  def fromAnnotation(tpe: Type, default: => Effect): Effect = fromAnnotation(tpe.finalResultType.annotations, default)

  def toAnnotation(eff: Effect): List[AnnotationInfo]

  def getterEffect(sym: Symbol): Effect = lattice.bottom
  def setterEffect(sym: Symbol): Effect = lattice.bottom
}

trait EffectLattice {
  type Effect

  def top: Effect
  def bottom: Effect

  def join(a: Effect, b: Effect): Effect
  def meet(a: Effect, b: Effect): Effect

  def lte(a: Effect, b: Effect): Boolean

  def joinAll(effs: Effect*): Effect = {
    if (effs.isEmpty) bottom
    else {
      var acc = bottom
      for (e <- effs) { acc = join(acc, e) }
      acc
    }
  }

  def meetAll(effs: Effect*): Effect = {
    if (effs.isEmpty) top
    else {
      var acc = top
      for (e <- effs) { acc = meet(acc, e) }
      acc
    }
  }

  implicit class EffectOps(eff: Effect) {
    def u(other: Effect): Effect = join(eff, other)
    def n(other: Effect): Effect = meet(eff, other)
    def <=(other: Effect): Boolean = lte(eff, other)
  }
}
