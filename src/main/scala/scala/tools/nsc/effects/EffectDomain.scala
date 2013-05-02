package scala.tools.nsc.effects

import scala.tools.nsc._

abstract class EffectDomain extends Infer with RelEffects with DefaultEffects with AnfTransform {
  // `global` should not be a class parameter. Having it a field allows to refine
  // its type, e.g. `EffectDomain { val global: some.global.type }`, which is not
  // possible for parameters.
  val global: Global

  val lattice: EffectLattice
  import lattice._

  import global._

  /**
   * A flag indicating if trees should be translated to ANF [*] before invoking
   * `computeEffect`. This flag can be overridden by concrete effect domains which
   * require trees to be in ANF, such as the purity checker.
   *
   * [*] Administrative Normal Form, see "The Essence of Compiling with Continuations"
   */
  val requireANF: Boolean = false

  /**
   * The annotation classes which are used by this effect domain to annotate effects.
   */
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

  /**
   * The effect represented by `annotations`
   */
  def fromAnnotationList(annots: List[AnnotationInfo]) = {
    val hasPureOrRel = annots.exists(ann => {
      val sym = ann.atp.typeSymbol
      sym == pureClass || sym == relClass
    })
    val default = if (hasPureOrRel) effectForPureAnnotated else top
    parseAnnotationInfos(annots, default)
  }

  /**
   * True there exists some effect annotations in the list `annotations`.
   */
  def existsEffectAnnotation(annotations: List[AnnotationInfo]) = {
    annotations.exists(ann => allEffectAnnots.contains(ann.atp.typeSymbol))
  }

  lazy val pureClass = rootMirror.getClassByName(newTypeName("scala.annotation.effects.pure"))

  lazy val allEffectAnnots = pureClass :: relClass :: annotationClasses

  /**
   * The effect that is assigned to getters and setters. This method can be overridden
   * by effect domains to assign a different effect to accessors.
   */
  def accessorEffect(sym: Symbol, tpe: Type, tree: Tree): Effect = bottom
}

trait EffectLattice {
  type Effect

  def top: Effect
  def bottom: Effect

  /**
   * This effect is used by the framework for methods that have a `@pure` annotation.
   * By default, pure methods have the `bottom` effect. Effect domains can override
   * this method to assign a different default effect to `@pure` methods.
   */
  def effectForPureAnnotated: Effect = bottom

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
