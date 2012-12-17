package scala.tools.nsc.effects

trait AnnotChecker { self: EffectChecker =>

  import global._
  import domain._
  import domain.lattice._

  global.addAnnotationChecker(new EffectAnnotationChecker())

  class EffectAnnotationChecker extends AnnotationChecker {


    // TODO: also need to handle relative annotations here

    def annotationsConform(tpe1: Type, tpe2: Type): Boolean = {
      val default = lattice.top
      val e1 = fromAnnotation(tpe1.annotations, default)
      val e2 = fromAnnotation(tpe2.annotations, default)
      e2 <= e1
    }

    override def assignAnnotationsToTree(defTree: Tree, typedRhs: Tree, tpe: Type): Type = defTree match {
      case DefDef(_, _, _, _, _, _) =>
        val e = domain.inferEffect(typedRhs, defTree.symbol)
        // TODO: add effect to type
        tpe

      case _ =>
        tpe
    }

    // todo: addannotations- copied from AnnotationTyper
    /*
    override def addAnnotations(tree: Tree, tpe: Type): Type = {
      if (tree.isTerm)
        removeAnnotations(tpe, annotationClasses)
      else
        tpe
    }
    */

    // todo: annotations-lub, annotations-glb
  }
}
