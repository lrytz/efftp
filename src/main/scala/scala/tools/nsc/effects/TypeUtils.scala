package scala.tools.nsc.effects

trait TypeUtils { self: EffectChecker =>

  import global._
  import domain._
  import lattice.Effect

  /**
   * Remove annotations with a type in `cls` from the type `tp`.
   */
  def removeAnnotations(tp: Type, cls: List[Symbol]): Type = tp match {
    case AnnotatedType(annots, underlying, _) =>
      underlying.withAnnotations(annots.filterNot(ann => cls.contains(ann.atp.typeSymbol)))
    case tp => tp
  }

  def setEffectAnnotation(tp: Type, eff: Effect): Type =
    removeAnnotations(tp, annotationClasses).withAnnotations(toAnnotation(eff))

  def setEffectAnnotation(tp: Type, eff: Effect, rel: List[RelEffect]): Type =
    setRelEffectAnnotations(setEffectAnnotation(tp, eff), rel)

  def setRelEffectAnnotations(tp: Type, rel: List[RelEffect]): Type =
    removeAnnotations(tp, List(relClass)).withAnnotations(relToAnnotation(rel))
}
