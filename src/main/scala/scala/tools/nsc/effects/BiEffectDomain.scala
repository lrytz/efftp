package scala.tools.nsc.effects

abstract class BiEffectDomain extends EffectDomain {
  import global._

  val d1: EffectDomain {
    val global: BiEffectDomain.this.global.type
  }

  val d2: EffectDomain {
    val global: BiEffectDomain.this.global.type
  }


  /* ******************************* *
   * Members from class EffectDomain *
   * ******************************* */

  override val requireANF: Boolean = d1.requireANF || d2.requireANF

  val lattice = new BiLattice {
    val l1: d1.lattice.type = d1.lattice
    val l2: d2.lattice.type = d2.lattice
  }
  import lattice.{Effect, BiEffect}
  
  val annotationClasses: List[Symbol] = d1.annotationClasses ++ d2.annotationClasses

  def parseAnnotationInfos(annots: List[AnnotationInfo], default: => Effect): Effect =
    BiEffect(d1.parseAnnotationInfos(annots, default.e1), d2.parseAnnotationInfos(annots, default.e2))

  def toAnnotation(eff: Effect): List[AnnotationInfo] = {
    d1.toAnnotation(eff.e1) ++ d2.toAnnotation(eff.e2)
  }

  override def accessorEffect(sym: Symbol, tpe: Type, tree: Tree): Effect =
    BiEffect(d1.accessorEffect(sym, tpe, tree), d2.accessorEffect(sym, tpe, tree))

    
    
    
    
  /* ************************ *
   * Members from trait Infer *
   * ************************ */
    

  /**
   * Most of the time effect errors are not reported using the reporter of the BiEffectDomain, and this
   * method is not invoked. The reason is that `computeEffectImpl` is invoked for each effect domain with
   * a reporter for only that domain, so if an effect error occurs within some expression, it is reported
   * by domain.
   * 
   * The BiEffectDomain reporter is only used if the expression passed to `computeEffect` produces an
   * effect mismatch itself, at top level. Example:
   * 
   *   def f(): Unit @pure @io = ()
   *   def g(): Unit @pure = f()
   *
   * In this case the `computeEffect` call for the expression `f()` at top level will produce the error
   * message. This call is performed by the framework on the active domain, i.e. the BiEffectDomain.
   */
  override def explainMismatch(expected: Effect, found: Effect): Option[String] = {
    def msg(found: String, expected: String) = s"$found does not conform to $expected"
    val msg1 = 
      if (lattice.l1.lte(found.e1, expected.e1)) None
      else d1.explainMismatch(expected.e1, found.e1).orElse(Some(msg(found.e1.toString, expected.e1.toString)))
    val msg2 = 
      if (lattice.l2.lte(found.e2, expected.e2)) None
      else d2.explainMismatch(expected.e2, found.e2).orElse(Some(msg(found.e2.toString, expected.e2.toString)))
    
    val l = (msg1.toList ::: msg2.toList)
    if (l.isEmpty) None
    else Some(l.mkString("\n"))
  }


  private def mkReporter(d: EffectDomain { val global: BiEffectDomain.this.global.type }, origCtx: EffectContext): d.EffectReporter = {
    new d.EffectReporter {
      def issueError(tree: Tree, msg: String) {
        origCtx.reporter.issueError(tree, msg)
      }
      def setError(tree: Tree) {
        origCtx.reporter.setError(tree)
      }
    }
  }
  
  // this is "a bit of" a waist - but we need to copy the RelEffects to the new domain, otherwise the case class instances,
  // e.g. `ParamLoc`, have an outer pointer to the wrong domain which breaks pattern matching.
  //
  // the better solution would be to pull RelEffect out of the EffectDomain, but that's not quite as straightforward.
  def mkRelEffs(d: EffectDomain { val global: BiEffectDomain.this.global.type }, origCtx: EffectContext): List[d.RelEffect] = {
    origCtx.relEnv map {
      case RelEffect(ParamLoc(param), funOpt) => d.RelEffect(d.ParamLoc(param), funOpt)
      case RelEffect(ThisLoc(cls), funOpt) => d.RelEffect(d.ThisLoc(cls), funOpt)
    }
  }

  private def d1Ctx(ctx: EffectContext): d1.EffectContext = {
    d1.EffectContext(
        ctx.expected.map(_.e1),
        mkRelEffs(d1, ctx),
        mkReporter(d1, ctx),
        ctx.errorInfo,
        ctx.patternMode)
  }
  private def d2Ctx(ctx: EffectContext): d2.EffectContext = {
    d2.EffectContext(
        ctx.expected.map(_.e2),
        mkRelEffs(d2, ctx),
        mkReporter(d2, ctx),
        ctx.errorInfo,
        ctx.patternMode)
  }

  override def computeEffectImpl(tree: Tree, ctx: EffectContext): Effect =
    BiEffect(d1.computeEffectImpl(tree, d1Ctx(ctx)), d2.computeEffectImpl(tree, d2Ctx(ctx)))


  override def adaptApplyEffect(fun: Symbol, funEff: Effect, byValEffs: Map[Symbol, Effect], repeatedEffs: Map[Symbol, List[Effect]], latent: Effect): Effect = {
    BiEffect(
        d1.adaptApplyEffect(fun, funEff.e1, byValEffs.mapValues(_.e1), repeatedEffs.mapValues(_.map(_.e1)), latent.e1),
        d2.adaptApplyEffect(fun, funEff.e2, byValEffs.mapValues(_.e2), repeatedEffs.mapValues(_.map(_.e2)), latent.e2))
  }
  
  override def adaptInferredPrimaryConstrEffect(
      constrDef: DefDef,
      rhsEff: Effect,
      fieldEffs: Map[Symbol, Effect],
      statEffs: List[Effect],
      traitParentConstrEffs: Map[Symbol, Effect]): Effect = {
    BiEffect(
        d1.adaptInferredPrimaryConstrEffect(constrDef, rhsEff.e1, fieldEffs.mapValues(_.e1), statEffs.map(_.e1), traitParentConstrEffs.mapValues(_.e1)),
        d2.adaptInferredPrimaryConstrEffect(constrDef, rhsEff.e2, fieldEffs.mapValues(_.e2), statEffs.map(_.e2), traitParentConstrEffs.mapValues(_.e2)))
  }

  override def adaptInferredMethodEffect(method: Symbol, eff: Effect) = {
    BiEffect(
        d1.adaptInferredMethodEffect(method, eff.e1),
        d2.adaptInferredMethodEffect(method, eff.e2))
  }
  
  override def adaptExpectedMethodEffect(method: Symbol, eff: Effect) = {
    BiEffect(
        d1.adaptExpectedMethodEffect(method, eff.e1),
        d2.adaptExpectedMethodEffect(method, eff.e2))
  }

}

trait BiLattice extends EffectLattice {
  val l1: EffectLattice
  val l2: EffectLattice

  type Effect = BiEffect
  
  case class BiEffect(val e1: l1.Effect, val e2: l2.Effect) {
    override def toString = s"$e1 $e2"
  }

  def top: Effect = BiEffect(l1.top, l2.top)
  def bottom: Effect = BiEffect(l1.bottom, l2.bottom)
  
  override def effectForPureAnnotated = BiEffect(l1.effectForPureAnnotated, l2.effectForPureAnnotated)

  def join(a: Effect, b: Effect): Effect = BiEffect(l1.join(a.e1, b.e1), l2.join(a.e2, b.e2))
  def meet(a: Effect, b: Effect): Effect = BiEffect(l1.meet(a.e1, b.e1), l2.meet(a.e2, b.e2))

  def lte(a: Effect, b: Effect): Boolean = l1.lte(a.e1, b.e1) && l2.lte(a.e2, b.e2)
}


/*
abstract class MultiEffectDomain extends EffectDomain {
  import global._

  private type SingleDomain = EffectDomain {
    val global: MultiEffectDomain.this.global.type
  }

  val domains: List[SingleDomain]

  val lattice = new MultiLattice(domains.map(_.lattice))
  import lattice.Effect

  val annotationClasses: List[Symbol] = domains.flatMap[Symbol, List[Symbol]](_.annotationClasses)

  def fromAnnotation(annots: List[AnnotationInfo]): Option[Effect] = {
    val domainEffs = domains.map(_.fromAnnotation(annots))
    if (domainEffs.exists(_.isEmpty)) None
    else Some(domainEffs.map(_.get))
  }

  def toAnnotation(elem: Effect): List[AnnotationInfo] = {
    (elem, domains).zipped.flatMap[AnnotationInfo, List[AnnotationInfo]]({
      case (eff, domain) =>
        type E = domain.lattice.Effect
        domain.toAnnotation(eff.asInstanceOf[E])
    })
  }

  override def getterEffect(sym: Symbol): Effect =
    domains.map(_.getterEffect(sym))

  override def setterEffect(sym: Symbol): Effect =
    domains.map(_.setterEffect(sym))

  override def computeRhsElem(rhs: Tree, sym: Symbol): Effect = {
    domains.map(_.computeRhsElem(rhs, sym))
  }
}

class MultiLattice(lattices: List[EffectLattice]) extends EffectLattice {

  type Effect = List[EffectLattice#Effect]

  def top: Effect =
    lattices.map(_.top)

  def bottom: Effect =
    lattices.map(_.bottom)

  def join(a: Effect, b: Effect) = {
    (a, b, lattices).zipped map {
      case (aEff, bEff, lattice) =>
        type E = lattice.Effect
        lattice.join(aEff.asInstanceOf[E], bEff.asInstanceOf[E])
    }
  }

  def meet(a: Effect, b: Effect) = {
    (a, b, lattices).zipped map {
      case (aEff, bEff, lattice) =>
        type E = lattice.Effect
        lattice.meet(aEff.asInstanceOf[E], bEff.asInstanceOf[E])
    }
  }

  def lte(a: Effect, b: Effect): Boolean = {
    (a, b, lattices).zipped forall {
      case (aEff, bEff, lattice) =>
        type E = lattice.Effect
        lattice.lte(aEff.asInstanceOf[E], bEff.asInstanceOf[E])
    }
  }
}
*/
