package colls

import scala.language.higherKinds
import annotation.effects._


/* ********** *
 * Exceptions *
 * ********** */

class NoSuchElem(msg: String) extends Exception {
//  annotations (if you want)
//  @pure @mod(this) @loc() type constructorEffect
}



/* ****** *
 * Option *
 * ****** */


trait Optn[+A] {
  def isEmpty: Boolean @pure

  def getOrElse[B >: A](default: => B): B @rel(default) =
    if (isEmpty) default else (get: @pure) // effect cast

  def get: A @pure @throws[NoSuchElem]
}
case class Som[+A](a: A) extends Optn[A] {
  def isEmpty = false
  def get = a
//  def get: A = a    // override error
}
case object Non extends Optn[Nothing] {
  def isEmpty = true
  def get = throw new NoSuchElem("Non.get")
}




/* ******* *
 * Builder *
 * ******* */


trait Bldr[-Elem, +To] {
  def +=(elem: Elem): this.type @pure @mod(this)
  def result(): To @pure
}

trait CBF[-From, -Elem, +To] {
  def apply(from: From): Bldr[Elem, To] @pure @loc()
}



/* *************** *
 * TraversableLike *
 * *************** */



trait TravLk[+A, +Repr] { self: Repr =>
  protected[this] def newBuilder: Bldr[A, Repr] @pure @loc()

  def foreach[U](f: A => U): Unit @rel(f)

  def isEmpty: Boolean @pure = {
    var result = true
//  cannot annotate masking behavior, need built-in support for `breakable` (and other library tools like `Try`)
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

  def head: A @pure @throws[NoSuchElem] = {
    var result: Optn[A] = Non
    for (x <- this) {
      if (result.isEmpty) result = Som(x)
    }
    result.getOrElse(throw new NoSuchElem(""))
  }

  def tail: Repr @pure @throws[NoSuchElem] = {
    if (isEmpty) throw new NoSuchElem("tail of empty traversable")
    drop(1)
  }

  def filter(p: A => Boolean): Repr @pure @rel(p) = {
    val b = newBuilder
    for (x <- this)
      if (p(x)) b += x
    b.result()
  }

  def map[B, That](f: A => B)(implicit bf: CBF[Repr, B, That]): That @pure @rel(f) = {
    val b = bf(self)
    for (x <- this) b += f(x)
    b.result()
  }
}




/* *********** *
 * Traversable *
 * *********** */


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




/* ******** *
 * Iterator *
 * ******** */

trait Itor[+A] {
  def hasNext: Boolean @pure
  def next(): A @pure @mod(this) @throws[NoSuchElem]
  def isEmpty: Boolean @pure = !hasNext
  def foreach[U](f: A => U): Unit @pure @mod(this) @rel(f) = {
    while (hasNext) f(next(): @pure @mod(this)) // effect cast for call to `next`
    ()
  }
}

object Itor {
  val empty = new Itor[Nothing] {
    def hasNext: Boolean @pure = false
    def next(): Nothing @pure @throws[NoSuchElem] = throw new NoSuchElem("next on empty iterator")
  }
}




/* ******** *
 * Iterable *
 * ******** */


trait ItrblLk[+A, +Repr] extends TravLk[A, Repr] { self: Repr =>
  def iterator: Itor[A] @pure @loc()
  def foreach[U](f: A => U): Unit @pure @rel(f) =
    iterator.foreach(f)
}

trait Itrbl[+A] extends Trav[A] with GenTravTmpl[A, Itrbl] with ItrblLk[A, Itrbl[A]] {
  override def companion: GenCpn[Itrbl] @pure = Itrbl
}
object Itrbl extends TravFct[Itrbl] {
  def newBuilder[A]: Bldr[A, Itrbl[A]] @pure @loc() = new LstBldr
}



/* *** *
 * Seq *
 * *** */

trait SqLk[+A, +Repr <: SqLk[A, Repr]] extends ItrblLk[A, Repr] { self: Repr =>
  def length: Int @pure
  def apply(idx: Int): A @pure @throws[NoSuchElem]

  def iterator: Itor[A] @pure @loc() = {
    new Itor[A] {
//      @pure @mod(this) @loc() type constructorEffect = Nothing
      var these = self // not @local!
      def hasNext: Boolean @pure = !these.isEmpty
      def next(): A @pure @throws[NoSuchElem] @mod(this) =
        if (hasNext) {
          val result = these.head
          these = these.tail        // no need to cast away the @throws[NoSuchElem] effect - allowed anyway
          result
        } else Itor.empty.next()
    }
  }

// same as above, but with a named class
  
//  def iterator: Itor[A] @pure @loc() = {
//    class Ann extends Itor[A] {
//      @pure @mod(this) @loc() type constructorEffect
//      var these = self // not @local!
//      def hasNext: Boolean @pure = !these.isEmpty
//      def next(): A @pure @throws[NoSuchElem] @mod(this) =
//        if (hasNext) {
//          val result = these.head
//          these = these.tail        // no need to cast away the @throws[NoSuchElem] effect - allowed anyway
//          result
//        } else Itor.empty.next()
//    }
//    new Ann
//  }

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





/* **** *
 * List *
 * **** */


// @pure @loc() 
sealed abstract class Lst[+A] extends Sq[A] with GenTravTmpl[A, Lst] with SqLk[A, Lst[A]] {
  def apply(idx: Int): A @pure @throws[NoSuchElem] = {
    if (idx == 0) head
    else tail(idx - 1)
  }

  def length: Int @pure = {
    if (isEmpty) 0
    else 1 + (tail: @pure).length // effect cast for `@throws[NoSuchElem]` of `tail`
  }

  override def companion: GenCpn[Lst] @pure = Lst
}

// @TODO: overriding a "def" using a "val" => makes it pure. do we need to annotate that?
final case class cns[A](override val head: A, override val tail: Lst[A]) extends Lst[A] {
//  optional constructor effect annotation
//  @pure @mod(this) @loc() type constructorEffect

  override def isEmpty = false
}

case object nl extends Lst[Nothing] {
  override def isEmpty = true
}


object Lst extends SqFct[Lst] {
  implicit def canBuildFrom[A]: CBF[Coll, A, Lst[A]] @pure = new GCBF[A]

   def apply[A](elems: A*): Lst[A] @pure = {
     // cast because it uses the scala.Seq
     elems.foldRight(nl: Lst[A])((a, res) => cns(a, res)): @pure
   }

  def newBuilder[A]: Bldr[A, Lst[A]] @pure @loc() = new LstBldr[A]

  override def empty[A]: Lst[A] @pure = nl
}


class LstBldr[A] extends Bldr[A, Lst[A]] {
//  optional constructor effect annotation
//  @pure @mod(this) @loc() type constructorEffect

  // @local val b = new collection.mutable.ListBuffer[A]() @TODO: annotated ListBuffer

  // does not need to be @local - we're not modifying any fields of the list, only the field of this builder
  var b: Lst[A] = nl

  def +=(a: A): this.type @pure @mod(this) = {
    // b += a // when using ListBuffer
    b = new cns(a, b)
    this
  }

  def result(): Lst[A] @pure = b
}

