package scala.tools.nsc.effects

import org.scalatest.FunSuite

class EffectLatticeSuite extends FunSuite {
  val s123 = S(1, 2, 3)
  val s1223 = ISLattice.join(S(1, 2), S(2, 3))

  test("ISLattice basics") {
    import ISLattice._
    assert(All <= top)
    assert(top <= (All u s123))
    assert(bottom <= All)
    assert(s1223 <= s123)
    assert(s123 <= s1223)
    assert((s123 n top) <= s123)
    assert((top n bottom) <= bottom)
  }

  test("ISLattice joinAll / meetAll") {
    import ISLattice._
    val ja1223 = joinAll(S(1), S(2), S(2), S(3))
    assert(ja1223 <= s123)
    assert(s1223 <= ja1223)

    val ma23 = meetAll(S(1, 2, 3), S(2, 3), All, S(2, 3, 4))
    assert(ma23 <= S(2, 3))
    assert(S(2, 3) <= ma23)
  }

  test("BiLattice basics") {
    import IOISL._
    import scala.tools.nsc.effects.{IOLattice => IOL, ISLattice => ISL}
    assert(bottom <= top)
    assert((IOL.bottom, ISL.bottom) <= bottom)
    assert(((NoIO, s123) u (SomeIO, ISL.bottom)) <= (SomeIO,s123))
  }
}

sealed trait IS
case object All extends IS
case class S(set: Set[Int]) extends IS {
  def this(xs: Int*) = this(Set(xs: _*))
}
object S {
  def apply(xs: Int*) = new S(xs: _*)
}

object ISLattice extends EffectLattice {
  type Effect = IS

  def top = All
  def bottom = S()

  def join(a: IS, b: IS) = (a, b) match {
    case (All, _) | (_, All) => All
    case (S(s1), S(s2)) => S(s1 union s2)
  }

  def meet(a: IS, b: IS) = (a, b) match {
    case (All, All) => All
    case (All, s) => s
    case (s, All) => s
    case (S(s1), S(s2)) => S(s1 intersect s2)
  }

  def lte(a: IS, b: IS) = (a, b) match {
    case (_, All) => true
    case (All, _) => false
    case (S(s1), S(s2)) => s1 subsetOf s2
  }
}


sealed trait IO
case object NoIO extends IO
case object SomeIO extends IO

object IOLattice extends EffectLattice {
  type Effect = IO

  def top = SomeIO
  def bottom = NoIO

  def join(a: IO, b: IO) = (a, b) match {
    case (NoIO, NoIO)     => NoIO
    case _                => SomeIO
  }

  def meet(a: IO, b: IO) = (a, b) match {
    case (SomeIO, SomeIO) => SomeIO
    case _                => NoIO
  }

  def lte(a: IO, b: IO) = (a, b) match {
    case (SomeIO, NoIO)   => false
    case _                => true
  }
}

object IOISL extends BiLattice {
  val l1: IOLattice.type = IOLattice
  val l2: ISLattice.type = ISLattice
}
