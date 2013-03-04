package scala.tools.nsc.effects
package state

import scala.tools.nsc.Global

abstract class StateLattice extends EffectLattice {
  val global: Global
  import global._

  type Effect = (Store, Locality)

  lazy val top: Effect    = (StoreAny, AnyLoc)
  lazy val bottom: Effect = (StoreLoc(), LocSet())

  lazy val noModAnyLoc: Effect = (StoreLoc(), AnyLoc)

  override def effectForPureAnnotated: Effect = noModAnyLoc

  def join(a: Effect, b: Effect): Effect =
    (joinStore(a._1, b._1), joinLocality(a._2, b._2))

  def meet(a: Effect, b: Effect): Effect =
    (meetStore(a._1, b._1), meetLocality(a._2, b._2))

  def lte(a: Effect, b: Effect): Boolean =
    lteStore(a._1, b._1) && lteLocality(a._2, b._2)



  def joinStore(a: Store, b: Store): Store = (a, b) match {
    case (StoreAny, _) | (_, StoreAny) =>
      StoreAny

    case (StoreLoc(as), StoreLoc(bs)) =>
      val merged = (Map[Location, LocSet]() /: as) {
        case (map, (location, aSet)) =>
          val res = bs.get(location).map(aSet.union).getOrElse(aSet)
          map + (location -> res)
      }
      val onlyInB = bs.filterNot(elem => as.contains(elem._1))
      StoreLoc(merged ++ onlyInB)
  }

  def joinLocality(a: Locality, b: Locality): Locality = (a, b) match {
    case (AnyLoc, _) | (_, AnyLoc) =>
      AnyLoc

    case (LocSet(as), LocSet(bs)) =>
      LocSet(as ++ bs)
  }



  def meetStore(a: Store, b: Store): Store = (a, b) match {
    case (StoreAny, b) => b
    case (a, StoreAny) => a

    case (StoreLoc(as), StoreLoc(bs)) =>
      val inBoth = as filter {
        case (location, _) => bs.contains(location)
      }
      val resMap = inBoth map {
        case (location, aSet) => (location, aSet.intersect(bs(location)))
      }
      StoreLoc(resMap)
  }

  def meetLocality(a: Locality, b: Locality): Locality = (a, b) match {
    case (AnyLoc, b) => b
    case (a, AnyLoc) => a

    case (LocSet(as), LocSet(bs)) =>
      LocSet(as intersect bs)
  }



  def lteStore(a: Store, b: Store) = (a, b) match {
    case (_, StoreAny) => true
    case (StoreAny, _) => false
    case (StoreLoc(as), StoreLoc(bs)) =>
      as.forall({
        case (location, aSet) => bs.get(location).map(bSet => aSet.s.subsetOf(bSet.s)).getOrElse(false)
      })
  }

  def lteLocality(a: Locality, b: Locality) = a.isFresh || ((a, b) match {
    case (_, AnyLoc) => true
    case (AnyLoc, _) => false
    case (LocSet(as), LocSet(bs)) =>
      as.subsetOf(bs)
  })



  /**
   * Locality of returned value
   */
  trait Locality {
    def isFresh = this match {
      case LocSet(s) =>
        s.isEmpty || s.toList == List(Fresh)

      case _ =>
        false
    }

//    def filterOutOfScope(ctx: Context): Locality
  }
  case object AnyLoc extends Locality {
//    def filterOutOfScope(ctx: Context) = AnyLoc
  }
  case class LocSet(s: Set[Location] = Set()) extends Locality {
    def this(l: Location) = this(Set(l))

    def union(b: LocSet)     = LocSet(s union b.s)
    def intersect(b: LocSet) = LocSet(s intersect b.s)
    def add(loc: Location)   = LocSet(s + loc)

//    def filterOutOfScope(ctx: Context) = LocSet(s.filter(_.isInScope(ctx)))
  }
  object LocSet {
    def apply(l: Location): LocSet = new LocSet(l)
  }



  /**
   * Locations, places that are subject to modification effects
   */
  sealed trait Location {
    /*
    def isLocalVar = this match {
      case SymLoc(sym) => sym.isLocal && sym.isVariable
      case ThisLoc(_) => false
      case Fresh => false
    }
    */

    /**
     * Note the difference for parameters and local values / variables:
     *  - a local variable defined in a method is out of scope outside that method
     *  - however, parameters also have the method as owner, but they are still in
     *    scope outside the method.
     */
    /*
    def isInScope(ctx: Context): Boolean = this match {
      case Fresh => true
      case ThisLocation(_) => true
      case SymLocation(sym) =>
        if (sym.isParameter)
          localIsInScope(sym, ctx)
        else
          localIsInScope(sym, ctx.outer)
    }
    */
  }
  case class SymLocation(sym: Symbol) extends Location {
    override def hashCode() = sym.hashCode
    override def equals(other: Any) = other match {
      case SymLocation(otherSym) =>
        sym.owner == otherSym.owner && sym.name == otherSym.name
      case _ =>
        false
    }
  }
  case class ThisLocation(sym: Symbol) extends Location
  case object Fresh extends Location



  /**
   * State modification effects
   */
  trait Store {
    def include(in: Location, from: Location): Store = include(in, LocSet(from))
    def include(in: Location, from: Locality): Store = this match {
      case StoreAny =>
        StoreAny
      case StoreLoc(effs) => from match {
        case AnyLoc => StoreAny
        case locs @ LocSet(_) =>
          StoreLoc(extendStoreMap(effs, in, locs))
      }
    }

    private def extendStoreMap(map: Map[Location, LocSet], storeIn: Location, storeFrom: LocSet) =
      map.updated(storeIn, map.get(storeIn).map(storeFrom.union).getOrElse(storeFrom))

    def isPure = this match {
      case StoreLoc(effs) =>
        effs.toList match {
          case Nil => true
          case List((in, from)) => in == Fresh && from.isFresh
          case _ => false
        }
      case _ => false
    }
  }

  case object StoreAny extends Store

  case class StoreLoc(effs: Map[Location, LocSet] = Map()) extends Store {
    def this(in: Location, from: LocSet) = this(Map(in -> from))
  }
  object StoreLoc {
    def apply(in: Location, from: LocSet) = new StoreLoc(in, from)
  }
}
