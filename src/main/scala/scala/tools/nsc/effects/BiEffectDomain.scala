package scala.tools.nsc.effects

abstract class BiEffectDomain extends EffectDomain {
  import global._

  type D1 <: EffectDomain {
    val global: BiEffectDomain.this.global.type
  }
  val d1: D1

  type D2 <: EffectDomain {
    val global: BiEffectDomain.this.global.type
  }
  val d2: D2

  val lattice = new BiLattice {
    val l1: d1.lattice.type = d1.lattice
    val l2: d2.lattice.type = d2.lattice
  }
  import lattice.Effect

  val annotationClasses: List[Symbol] = d1.annotationClasses ++ d2.annotationClasses

  def fromAnnotation(annots: List[AnnotationInfo], default: => Effect): Effect =
    (d1.fromAnnotation(annots, default._1), d2.fromAnnotation(annots, default._2))

  def toAnnotation(eff: Effect): List[AnnotationInfo] = {
    d1.toAnnotation(eff._1) ++ d2.toAnnotation(eff._2)
  }

  override def getterEffect(sym: Symbol): Effect =
    (d1.getterEffect(sym), d2.getterEffect(sym))

  override def setterEffect(sym: Symbol): Effect =
    (d1.setterEffect(sym), d2.setterEffect(sym))


  override def inferEff(tree: Tree, enclFun: Symbol, set: Effect => Unit, continue: => Unit) {
    var e1 = Option.empty[d1.lattice.Effect]
    var e2 = Option.empty[d2.lattice.Effect]
    d1.inferEff(tree, enclFun, e => {e1 = Some(e)}, ())
    d2.inferEff(tree, enclFun, e => {e2 = Some(e)}, ())

    (e1, e2) match {
      case (None, None) =>
        continue
      case _ =>
        set(e1.getOrElse(d1.inferEffect(tree, enclFun)), e2.getOrElse(d2.inferEffect(tree, enclFun)))
    }
  }
}

trait BiLattice extends EffectLattice {
  val l1: EffectLattice
  val l2: EffectLattice

  type Effect = (l1.Effect, l2.Effect)

  def top: Effect = (l1.top, l2.top)
  def bottom: Effect = (l1.bottom, l2.bottom)

  def join(a: Effect, b: Effect): Effect = (l1.join(a._1, b._1), l2.join(a._2, b._2))
  def meet(a: Effect, b: Effect): Effect = (l1.meet(a._1, b._1), l2.meet(a._2, b._2))

  def lte(a: Effect, b: Effect): Boolean = l1.lte(a._1, b._1) && l2.lte(a._2, b._2)
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
