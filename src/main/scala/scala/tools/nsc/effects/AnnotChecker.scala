package scala.tools.nsc.effects

trait AnnotChecker { self: EffectChecker =>

  import global._
  import domain._
  import domain.lattice._

  global.addAnnotationChecker(new EffectAnnotationChecker())

  class EffectAnnotationChecker extends AnnotationChecker {


    // TODO: also need to handle relative annotations here
    // problem: we need to add @rel annotations to all methods, we can't always look at the corresponding
      // symbol: when comparing two types, as here, subtyping doesn't carry symbols along. subtyping is
    // only correct when @rel annotations are there, so we need to add them. however, we want to do that
    // probably only on completion of the symbol's type, not the first time we need to check the @rel (that
    // might lead to cyclic refs)

    def annotationsConform(tpe1: Type, tpe2: Type): Boolean = {
      val default = lattice.top
      val e1 = fromAnnotation(tpe1.annotations, default)
      val e2 = fromAnnotation(tpe2.annotations, default)
      e2 <= e1
    }

    override def assignAnnotationsToTree(defTree: Tree, typedRhs: Tree, tpe: Type): Type = defTree match {
      case DefDef(_, _, _, _, _, _) =>
        val sym = defTree.symbol
        val e = domain.inferEffect(typedRhs, sym)
        val rel = domain.relEffects(sym)
        setEffectAnnotation(tpe, e, rel)

      case _ =>
        tpe
    }

    override def addAnnotations(tree: Tree, tpe: Type): Type =
      if (tree.isTerm)
        removeAnnotations(tpe, relClass :: annotationClasses)
      else
        tpe

    // todo: annotations-lub, annotations-glb
  }
}
