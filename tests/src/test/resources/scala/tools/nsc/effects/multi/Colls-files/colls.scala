package colls

import scala.language.higherKinds
import annotation.effects._

trait Optn[+A] {
  def isEmpty: Boolean @pure

  def getOrElse[B >: A](default: => B): B @rel(default) =
    if (isEmpty) default else (get: @pure) // effect cast

  def get: A @pure @throws[NoSuchElementException]
}
case class Som[+A](a: A) extends Optn[A] {
  def isEmpty = false
  def get: A = a
}
case object Non extends Optn[Nothing] {
  def isEmpty = true
  def get = throw new NoSuchElementException()
}

trait Bldr[-Elem, +To] {
  def +=(elem: Elem): this.type @pure @mod(this)
  def result(): To @pure
}

trait CBF[-From, -Elem, +To] {
  def apply(from: From): Bldr[Elem, To] @pure @loc()
}

trait TravLk[+A, +Repr] { self: Repr =>
  protected[this] def newBuilder: Bldr[A, Repr] @pure @loc()

  def foreach[U](f: A => U): Unit @rel(f)

  def isEmpty: Boolean @pure = {
    var result = true
    // cannot annotate masking behavior, need built-in support for `breakable` (and other library tools like `Try`)
//  breakable {
      for (x <- this) {
        result = false
//      break
      }
//  }
    result
  }

  def drop(n: Int): Repr @pure = {
    val b = newBuilder
    var i = 0
    for (x <- this) {
      if (i >= n) b += x
      i += 1
    }
    b.result()
  }

  def head: A @pure @throws[NoSuchElementException] = {
    var result: Optn[A] = None
    for (x <- this) {
      // need cast for `isEmpty`, constructor `Some`
      if (result.isEmpty: @pure) result = (Som(x): @pure)
    }

    // need cast for `getOrElse`
    result.getOrElse(throw new NoSuchElementException): @pure @throws[NoSuchElementException]
  }

  def tail: Repr @pure @throws[UnsupportedOperationException] = {
    // need cast for UOE constructor
    if (isEmpty) throw (new UnsupportedOperationException("empty.tail"): @pure)
    drop(1)
  }

  def filter(p: A => Boolean): Repr @pure @rel(p.apply(%)) = {
    val b = newBuilder
    for (x <- this)
      if (p(x)) b += x
    b.result()
  }

  def map[B, That](f: A => B)(implicit bf: CBF[Repr, B, That]): That @pure @rel(f.apply(%)) = {
    val b = bf(self)
    for (x <- this) b += f(x)
    b.result()
  }
}





trait GenTravTmpl[+A, +CC[X] <: Trav[X]] {
  def companion: GenCpn[CC] @pure
  protected[this] def newBuilder: Bldr[A, CC[A]] @pure @loc() = companion.newBuilder[A]
  def genericBuilder[B]: Bldr[B, CC[B]] @pure @loc() = companion.newBuilder[B]
}

abstract class GenCpn[+CC[X] <: Trav[X]] {
  type Coll = CC[_]
  def newBuilder[A]: Bldr[A, CC[A]] @pure @loc()
  def empty[A]: CC[A] @pure = newBuilder[A].result()
}

abstract class TravFct[CC[X] <: Trav[X] with GenTravTmpl[X, CC]] extends GenCpn[CC] {
  @pure @loc() class GCBF[A] extends CBF[CC[_], A, CC[A]] {
    def apply(from: Coll): Bldr[A, CC[A]] @pure @loc() = from.genericBuilder[A]
  }
}

trait Trav[+A] extends TravLk[A, Trav[A]] with GenTravTmpl[A, Trav] {
  def companion: GenCpn[Trav] @pure = Trav
}

object Trav extends TravFct[Trav] {
  implicit def canBuildFrom[A]: CBF[Coll, A, Trav[A]] @pure = new GCBF[A]

  def newBuilder[A]: Bldr[A, Trav[A]] @pure @loc() = new LstBldr
}





trait Itor[+A] {
  def hasNext: Boolean @pure
  def next(): A @pure @mod(this) @throws[NoSuchElementException]
  def isEmpty: Boolean @pure = !hasNext
  def foreach[U](f: A => U): Unit @pure @mod(this) @rel(f.apply(%)) = {
    while (hasNext) f(next()) // @TODO: mask NSE
    ()
  }
}

object Itor {
  val empty = new Itor[Nothing] {
    def hasNext: Boolean @pure = false
    def next(): Nothing @pure @throws[NoSuchElementException] = throw (new NoSuchElementException("next on empty iterator"): @pure)
  }
}







trait ItrblLk[+A, +Repr] extends TravLk[A, Repr] { self: Repr =>
  def iterator: Itor[A] @pure @loc()
  def foreach[U](f: A => U): Unit @pure @rel(f.apply(%)) =
    iterator.foreach(f)
}

trait Itrbl[+A] extends Trav[A] with GenTravTmpl[A, Itrbl] with ItrblLk[A, Itrbl[A]] {
  override def companion: GenCpn[Itrbl] @pure = Itrbl
}
object Itrbl extends TravFct[Itrbl] {
  def newBuilder[A]: Bldr[A, Itrbl[A]] @pure @loc() = new LstBldr
}



trait SqLk[+A, +Repr <: SqLk[A, Repr]] extends ItrblLk[A, Repr] { self: Repr =>
  def length: Int @pure
  def apply(idx: Int): A @pure @throws[NoSuchElementException | UnsupportedOperationException]

  def iterator: Itor[A] @pure @loc() = {
    class Ann extends Itor[A] {
      @pure @mod(this) @loc() type constructorEffect
      var these = self // not @local!
      def hasNext: Boolean @pure = !these.isEmpty
      def next(): A @pure @throws[NoSuchElementException] /*@throws[UnsupportedOperationException]*/ @mod(this) =
        if (hasNext) {
          // TODO: if(hasNext) => "tail" will not throw the UnsupportedOperationException...
          val result = these.head; these = these.tail; result
        } else Itor.empty.next()
    }
    new Ann
  }
    /* @TODO: the constructor of the anonymous class is not inferred, resulting in an effect error (need a fresh value!)     new Itor[A] {
    var these = self // not @local!
    def hasNext: Boolean @pure = !these.isEmpty
    def next(): A @pure @throws[NoSuchElementException] /*@throws[UnsupportedOperationException]*/ @mod(this) =
      if (hasNext) {
        // TODO: if(hasNext) => "tail" will not throw the UnsupportedOperationException...
        val result = these.head; these = these.tail; result
      } else Itor.empty.next()
  }*/
}



abstract class SqFct[CC[X] <: Sq[X] with GenTravTmpl[X, CC]] extends TravFct[CC] {
  // adds "unapplySeq" in real; otherwise not needed, could use TravFct
}

trait Sq[+A] extends Itrbl[A] with GenTravTmpl[A, Sq] with SqLk[A, Sq[A]] {
  override def companion: GenCpn[Sq] @pure = Sq
}

object Sq extends SqFct[Sq] {
  def newBuilder[A]: Bldr[A, Sq[A]] @pure @loc() = new LstBldr
}



@pure @loc() sealed abstract class Lst[+A] extends Sq[A] with GenTravTmpl[A, Lst] with SqLk[A, Lst[A]] {
  def apply(idx: Int): A @pure @throws[NoSuchElementException | UnsupportedOperationException] = if (idx == 0) head else tail(idx - 1)
  def length: Int @pure = if (isEmpty) 0 else 1+tail.length
  override def companion: GenCpn[Lst] @pure = Lst
}

// @TODO: overriding a "def" using a "val" => makes it pure. do we need to annotate that?
final case class cns[A](override val head: A, override val tail: Lst[A]) extends Lst[A] {
  @pure @mod(this) @loc() type constructorEffect
  override def isEmpty: Boolean @pure = false
}

case object nl extends Lst[Nothing] {
  override def isEmpty: Boolean @pure = true
}

class LstBldr[A] extends Bldr[A, Lst[A]] {
  @pure @mod(this) @loc() type constructorEffect
  // @local val b = new collection.mutable.ListBuffer[A]() @TODO
  var b: Lst[A] = nl
  def +=(a: A): this.type @pure @mod(this) = {
    // b += a // @mod(this); need to know that ListBuffer.+= has effect @mod(this)
    b = new cns(a, b)
    this
  }
  def result(): Lst[A] @pure = b // Lst(b: _*)
}

object Lst extends SqFct[Lst] {
  implicit def canBuildFrom[A]: CBF[Coll, A, Lst[A]] @pure = new GCBF[A]

  // @TODO
  // def apply[A](elems: A*): Lst[A] @pure = {
    // elems.foldRight(nl: Lst[A])((a, res) => cns(a, res))
  // }
  def newBuilder[A]: Bldr[A, Lst[A]] @pure @loc() = new LstBldr[A]
  override def empty[A]: Lst[A] @pure = nl
}

