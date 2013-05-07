package collsOld

trait BtStLk[+This <: BtStLk[This] with St[Int]] extends StLk[Int, This] { self: This =>
  def empty: This
}

trait BtStFct[Coll <: BtSt with BtStLk[Coll]] {
  def empty: Coll
  def newBuilder: Bldr[Int, Coll]
  def bitsetCanBuildFrom = new CBF[Coll, Int, Coll] {
    def apply(from: Coll) = newBuilder
  }
}

abstract class BtSt extends St[Int] with BtStLk[BtSt] {
  override def empty: BtSt = BtSt.empty
}

object BtSt extends BtStFct[BtSt] {
  val empty = new BtStImpl()
  def newBuilder: Bldr[Int, BtSt] = new AddBldr[Int, BtSt](empty)
  implicit def canBuildFrom: CBF[BtSt, Int, BtSt] = bitsetCanBuildFrom
}



class BtStImpl(private val els: collection.immutable.Set[Int] = new collection.immutable.HashSet[Int]()) extends BtSt { self =>
  def contains(elem: Int): Boolean = els(elem)
  def + (elem: Int): BtSt = new BtStImpl(self.els + elem)
  def - (elem: Int): BtSt = new BtStImpl(self.els - elem)
  def iterator: Itor[Int] = new Itor[Int] {
    val it = els.iterator
    def hasNext: Boolean = it.hasNext
    def next(): Int = it.next()
  }
  override def foreach[U](f: Int =>  U): Unit = els.foreach[U](f)
}
