package scala.tools.nsc.effects
package purity

import scala.tools.nsc.Global

trait PurityLattice extends EffectLattice {
  val global: Global
  import global._

  type Mod = Locality
  type ResLoc = Locality

  type Effect = (Mod, ResLoc)

  lazy val top: Effect            = (AnyLoc, AnyLoc)
  lazy val bottom: Effect         = (RefSet(), RefSet())
  lazy val noModAnyResLoc: Effect = (RefSet(), AnyLoc)

  override def effectForPureAnnotated: Effect = noModAnyResLoc


  def join(a: Effect, b: Effect): Effect =
    (a._1 union b._1, a._2 union b._2)

  def meet(a: Effect, b: Effect): Effect =
    (a._1 intersect b._1, a._2 intersect b._2)

  def lte(a: Effect, b: Effect): Boolean =
    (a._1 subsetOf b._1) && (a._2 subsetOf b._2)





  sealed trait VarRef
  case class SymRef(sym: Symbol) extends VarRef {
    override def hashCode() = sym.hashCode
    override def equals(other: Any) = other match {
      case SymRef(otherSym) =>
        sym.owner == otherSym.owner && sym.name == otherSym.name
      case _ =>
        false
    }
  }
  case class ThisRef(sym: Symbol) extends VarRef


  trait Locality {
    def union(b: Locality): Locality = (this, b) match {
      case (AnyLoc, _) | (_, AnyLoc) =>
        AnyLoc
      case (RefSet(as), RefSet(bs)) =>
        RefSet(as union bs)
    }

    def intersect(b: Locality): Locality = (this, b) match {
      case (AnyLoc, b) => b
      case (a, AnyLoc) => a
      case (RefSet(as), RefSet(bs)) =>
        RefSet(as intersect bs)
    }

    def subsetOf(b: Locality): Boolean = (this, b) match {
      case (_, AnyLoc) => true
      case (AnyLoc, _) => false
      case (RefSet(as), RefSet(bs)) =>
        as.subsetOf(bs)
    }
  }
  case object AnyLoc extends Locality
  case class RefSet(s: Set[VarRef] = Set()) extends Locality {
    def this(l: VarRef) = this(Set(l))
    def add(loc: VarRef)     = RefSet(s + loc)
  }
  object RefSet {
    def apply(l: VarRef): RefSet = new RefSet(l)
  }


}
