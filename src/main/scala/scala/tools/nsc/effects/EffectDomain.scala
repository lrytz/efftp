package scala.tools.nsc.effects

import scala.tools.nsc._

abstract class EffectDomain extends Infer with RelEffects with DefaultEffects {
  // `global` should not be a class parameter. Having it a field allows to refine
  // its type, e.g. `EffectDomain { val global: some.global.type }`, which is not
  // possible for parameters.
  val global: Global

  val lattice: EffectLattice
  import lattice._

  import global._

  val annotationClasses: List[Symbol]

  /**
   * Read the effect from a list of annotations. If no effect annotation for this domain
   * can be found, this method should return the `default` effect.
   *
   * This method does not need to handle `@pure` or `@rel` annotations: this is already
   * done by `fromAnnotations`.
   */
  def parseAnnotationInfos(annots: List[AnnotationInfo], default: => Effect): Effect

  /**
   * TODO: rename to be more in line with `parseAnnotationInfos`.
   * Maybe rename both. (`parseEffectFromAnnotations`, `emitEffectAsAnnotations`)
   */
  def toAnnotation(eff: Effect): List[AnnotationInfo]

  /**
   * The effect represented by the annotations on the (return type of the potential method type) `tpe`.
   *
   * If there is no effect annotation for the specific domain, the default effect is used:
   *  - `bottom` if there is a `@pure` or a `@rel(...)` annotation
   *  - `top` otherwise
   */
  def fromAnnotation(tpe: Type): Effect = fromAnnotationList(tpe.finalResultType.annotations)

  def fromSymAnnotation(sym: Symbol): Effect = fromAnnotationList(sym.annotations)

  private def fromAnnotationList(annots: List[AnnotationInfo]) = {
    val hasPureOrRel = annots.exists(ann => {
      val sym = ann.atp.typeSymbol
      sym == pureClass || sym == relClass
    })
    val default = if (hasPureOrRel) bottom else top
    parseAnnotationInfos(annots, default)
  }

  def hasEffectAnnotations(tpe: Type): Boolean =
    tpe.finalResultType.annotations.exists(allEffectAnnots.contains)

  def symHasEffectAnnotations(sym: Symbol): Boolean =
    sym.annotations.exists(ann => allEffectAnnots.contains(ann.atp.typeSymbol))

  lazy val pureClass = rootMirror.getClassByName(newTypeName("scala.annotation.effects.pure"))

  lazy val allEffectAnnots = pureClass :: relClass :: annotationClasses

  def accessorEffect(sym: Symbol): Effect = bottom
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
