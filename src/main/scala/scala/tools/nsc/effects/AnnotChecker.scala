package scala.tools.nsc.effects

trait AnnotChecker { self: EffectChecker =>

  import global._
  import typer.infer.setError
  import domain._
  import domain.lattice._

  global.addAnnotationChecker(new EffectAnnotationChecker())

  class EffectAnnotationChecker extends AnnotationChecker {

    // @TODO: make sure we end up here only when checking refinements, members of types. effects of method return
    // types for instance should not be checked here. how? maybe inspect the stack trace to get some evidence.

    def annotationsConform(tpe1: Type, tpe2: Type): Boolean = {
      val e1 = fromAnnotation(tpe1)
      val e2 = fromAnnotation(tpe2)

      val rel1 = relFromAnnotation(tpe1)
      val rel2 = relFromAnnotation(tpe2)

      effectsConform(e1, rel1, e2, rel2)._1
    }

    private def effectsConform(e1: Effect, rel1: List[RelEffect],
                               e2: Effect, rel2: List[RelEffect],
                               visited: Set[Symbol] = Set()): (Boolean, Set[Symbol]) = {
      var localVisited = visited

      val res = e1 <= e2 && rel1.forall(r1 => {
        lteRel(List(r1), rel2) || {
          r1 match {
            case RelEffect(loc, Some(fun)) =>
              if (localVisited(fun)) true
              else {
                val resTp = fun.info.finalResultType
                val eFun = fromAnnotation(resTp)
                val relFun = relFromAnnotation(resTp)
                val (b, v) = effectsConform(eFun, relFun, e2, rel2, localVisited)
                localVisited = v
                b
              }

            case _ =>
              // @TODO: here we'd need to get all methods of the type of `loc` and check if their effects conform
              // @TODO: should issue a implementation restriction warning
              lattice.top <= e2
          }
        }
      })
      (res, localVisited)
    }


    /**
     * annotations-lub, annotations-glb, for effects in refinements: e.g. if(..) fun1 else fun2, we need
     * lub of the fun's effects
     *
     * In fact, with way "lub" is written in the Scala compiler currently, it's impossible to trigger these
     * methods. First, remember that effect annotations can only appear on return types of methods. Therefore
     * a lub of two effects can only be triggered when computing the lub of two method types, which is only
     * possible if the method appears in a refined type.
     *
     * The type of a term never has an effect annotation, so for instance in
     *
     *   if (cond) t1 else t2
     *
     * the types of t1 and t2 will not have any effect annotations - but they might be refined types with
     * effect-annotated methods.
     *
     * When computing the lub, the compiler first checks if one type is a supertype of all others (elimSub) and
     * keeps that as the resulting LUB. In this case, effect annotations are preserved.
     *
     * Otherwise, it triggers the actual lub computation. Without going into details of "def lub1", it seems that
     * the scala compiler doesn't compute refinements of individual members at all, as shown by this repl transcript
     *
     *   scala> class A; class B
     *   scala> if (cnd) new { def f: A = new A } else new { def f: B = new B }
     *   res2: Object = $anon$1@9aa3f3
     *
     * The type of res2 is Object, even though it could be { def f: Object }
     *
     * Note that there's a workaround: manually giving the expected type. Then the lub computation is not triggered,
     * instead the typer will just verify that both branches of the "if" conform to the expected type
     *
     *   scala> (if (cnd) new { def f: A = new A } else new { def f: B = new B }) : { def f: Object }
     *   res3: AnyRef{def f: Object} = $anon$2@457235
     */

    override def annotationsLub(tp: Type, ts: List[Type]): Type =
      lubOrGlb(tp, ts, joinAll, joinAllRel)

    override def annotationsGlb(tp: Type, ts: List[Type]): Type =
      lubOrGlb(tp, ts, meetAll, meetAllRel)


    // @TODO: should only do something if there are effect annotations in "ts", the method is also triggered
    // if there are other kinds of annotations
    def lubOrGlb(tp: Type, ts: List[Type],
                 combineEff: List[Effect] => Effect,
                 combineRel: List[List[RelEffect]] => List[RelEffect]): Type = {
      val effs = ts.map(tp => fromAnnotation(tp))
      // @TODO: check if rel effects all refer to symbols of the resulting method type, i.e. if lub/glb unify the
      // symbols for dependent method types
      val rels = ts.map(tp => relFromAnnotation(tp))
      val eff = combineEff(effs)
      val rel = combineRel(rels)
      setEffectAnnotation(tp, eff, rel)
    }


    /**
     * We remove all effect annotations from the expected type when type-checking a tree.
     *
     * @TODO: doc why, see session.md
     */
    override def annotationsPt(tree: Tree, mode: Int, pt: Type): Type = {
      removeAnnotations(pt, allEffectAnnots)
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
          val funSym = tree.symbol
          val enclMeth = funSym.enclMethod
          val e = domain.inferEffect(body, enclMeth)
          val enclRel = domain.relEffects(enclMeth)

          // we also compute the effect of the function body assuming there are no relative effects.
          // if that effect is the same (smaller or equal) as the effect e, it means that the body
          // of the function does not actually have the relative effect of the enclosing method.
          // therefore we don't assign any relative effect to the function in that case.
          val effectiveRel =
            if (enclRel.isEmpty) Nil
            else {
              val eNoRel = domain.inferEffect(body, NoSymbol)
              if (eNoRel <= e) Nil else enclRel
            }

          // @TODO: not sure what the owner of the new refinement symbol should be (funSym.enclClass)?
          // @TODO: should also remove effect annotations from tpe (as in the default case below)? probably yes.
          updateFunctionTypeEffect(tpe, e, effectiveRel, funSym.enclClass, tree.pos)

        case _ =>
          removeAnnotations(tpe, allEffectAnnots)

      } else {
        tree match {
          case DefDef(_, _, _, _, tpt @ TypeTree(), rhs) if !tpt.wasEmpty =>
            if (!rhs.isErroneous) {
              val sym = tree.symbol
              val rhsEff = domain.inferEffect(rhs, sym)
              val annotEff = fromAnnotation(sym.tpe)
              if (!(rhsEff <= annotEff))
                issueEffectError(rhs, rhsEff, annotEff)
            }

          case _ =>

        }
        tpe
      }
  }


  def issueEffectError(tree: Tree, found: Effect, expected: Effect) {
    val msg = "effect type mismatch;\n found   : " + found + "\n required: " + expected
    analyzer.ErrorUtils.issueTypeError(analyzer.NormalTypeError(tree, msg))(typer.context)
    setError(tree)
  }
}
