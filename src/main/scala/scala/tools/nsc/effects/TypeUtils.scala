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

  def setEffectAnnotation(tp: Type, eff: Effect): Type = {
    removeAnnotations(tp, annotationClasses).withAnnotations(toAnnotation(eff))
  }
}
