package scala.tools.nsc.effects

trait AnnotChecker { self: EffectChecker =>

  import global._
  import typer.infer.setError
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
      e1 <= e2
    }

    /**
     * We remove all effect annotations from the expected type when type-checking a tree.
     *
     * @TODO: doc why, see session.md
     */
    override def annotationsPt(tree: Tree, mode: Int, pt: Type): Type = {
      removeAnnotations(pt, annotationClasses)
    }

    override def assignAnnotationsToTree(defTree: Tree, typedRhs: Tree, tpe: Type): Type = defTree match {
      case _: DefDef =>
        val sym = defTree.symbol
        val e = domain.inferEffect(typedRhs, sym)
        val rel = domain.relEffects(sym)
        setEffectAnnotation(tpe, e, rel)

      case _ =>
        tpe
    }

    override def addAnnotations(tree: Tree, tpe: Type): Type =
      if (tree.isTerm) tree match {
        case Function(params, body) =>
          val enclMeth = tree.symbol.enclMethod
          val e = domain.inferEffect(body, enclMeth)
          // @TODO: not sure what the owner of the new refinement symbol should be (tree.symbol.enclClass)?
          updateFunctionTypeEffect(tpe, e, tree.symbol.enclClass, tree.pos)

        case _ =>
          removeAnnotations(tpe, relClass :: annotationClasses)

      } else {
        tree match {
          case DefDef(_, _, _, _, tpt @ TypeTree(), rhs) if !tpt.wasEmpty =>
            val sym = tree.symbol
            val rhsEff = domain.inferEffect(rhs, sym)
            val annotEff = fromAnnotation(sym.tpe.finalResultType, top)
            if (!(rhsEff <= annotEff))
              issueEffectError(rhs, rhsEff, annotEff)

          case _ =>

        }
        tpe
      }

    // todo: annotations-lub, annotations-glb
  }


  def issueEffectError(tree: Tree, found: Effect, expected: Effect) {
    val msg = "effect type mismatch;\n found   : " + found + "\n required: " + expected
    analyzer.ErrorUtils.issueTypeError(analyzer.NormalTypeError(tree, msg))(typer.context)
    setError(tree)
  }
}
