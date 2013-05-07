package collsOld

import annotation.effects._

trait Addable[A, +Repr <: Addable[A, Repr]] { self: Repr =>
  def +(elem: A): Repr @pure
}

class AddBldr[Elem, To <: Addable[Elem, To] with Itrbl[Elem] with ItrblLk[Elem, To]](empty: To) extends Bldr[Elem, To] {
  @pure @mod(this) @loc() type constructorEffect
  protected var elems: To = empty
  def +=(x: Elem): this.type @pure @mod(this) = { elems = elems + x; this }
  def clear(): Unit @pure @mod(this) = { elems = empty }
  def result(): To @pure = elems
}



trait StLk[A, +This <: StLk[A, This] with St[A]] extends ItrblLk[A, This] with Addable[A, This] { self: This =>
  def empty: This @pure
  override protected[this] def newBuilder: Bldr[A, This] @pure @loc() = new AddBldr[A, This](empty)
  def contains(elem: A): Boolean @pure
  def + (elem: A): This @pure
  def - (elem: A): This @pure
  def apply(elem: A): Boolean @pure = contains(elem)
}

abstract class StFct[CC[X] <: St[X] with StLk[X, CC[X]]] extends GenCpn[CC] {
  def newBuilder[A]: Bldr[A, CC[A]] @pure @loc() = new AddBldr[A, CC[A]](empty[A])
  def setCanBuildFrom[A] = {
	@pure @loc() class Ann extends CBF[CC[_], A, CC[A]] {
      def apply(from: CC[_]): Bldr[A, CC[A]] @pure @loc() = newBuilder[A]
    }
	new Ann
  }
}

trait St[A] extends (A => Boolean) with Itrbl[A] with GenTravTmpl[A, St] with StLk[A, St[A]] {
  override def companion: GenCpn[St] @pure = St
}

object St extends StFct[St] {
  override def empty[A]: St[A] @pure = new HSt[A]()
  implicit def canBuildFrom[A]: CBF[Coll, A, St[A]] = setCanBuildFrom[A]
}



class HSt[A](private val els: collection.immutable.Set[A] = new collection.immutable.HashSet[A]()) extends St[A] with GenTravTmpl[A, HSt] with StLk[A, HSt[A]] { self =>
  @pure @mod(this) @loc() type constructorEffect
  override def companion: GenCpn[HSt] @pure = HSt

  /**
   * The real collections define GenStTmpl, and provide an implementation for
   * "def empty" there. However, this doesn't allow StLk to type check. With a
   * concrete method in GenStTmpl
   *   def empty: CC[A] = companion.empty[A]
   *
   * The concrete method will override the abstract one in StLk
   *   def empty: This
   *
   * and the type of "empty" in StLk is CC[A], i.e. Set[A], not This. So
   * the constructor call "new AddBldr[A, This](empty)" fails, because it
   * expects a This, but gets a Set[A].
   */
  def empty: HSt[A] @pure = companion.empty[A]
  def contains(elem: A): Boolean @pure = els(elem)
  def + (elem: A): HSt[A] @pure = new HSt[A](self.els + elem)
  def - (elem: A): HSt[A] @pure = new HSt[A](self.els - elem)
  def iterator: Itor[A] = new Itor[A] {
    @local val it = els.iterator
    def hasNext: Boolean @pure = it.hasNext
    def next(): A @pure @mod(this) = it.next()
  }
}

object HSt extends StFct[HSt] {
  implicit def canBuildFrom[A]: CBF[Coll, A, HSt[A]] = setCanBuildFrom[A]
  override def empty[A]: HSt[A] @pure = new HSt[A]()
}
