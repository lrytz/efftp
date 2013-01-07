package scala.tools.nsc.effects.io

import org.scalatest.FunSuite
import scala.tools.nsc.effects.testing.TestMacros._

import scala.annotation.effects._

class IOSuite extends FunSuite {
  def foo(): Unit @io = { println() }

  // need local values, see below
  val withIO = new { def f() = foo() }
  val withoutIO = new { def f(): Unit @noIo = () }

  test("method with io") {
    assert(isSubtype[{ def f(): Unit @io }](withIO))
    assert(!isSubtype[{ def f(): Unit @noIo }](withIO))
    assert(isSubtype[{ def f(): Unit @io }](withoutIO))
    assert(isSubtype[{ def f(): Unit @noIo }](withoutIO))
  }

  test("type inference does not keep refinements when expected type is any") {
    // due to some special case with anonymous classes and refinement types, see
    // typedBlock in Typers.scala
    // this makes the inferred type of "new { ...}" only "Object{}", the refined
    // member is dropped because the expected type is Any, so it would not be
    // accessible anyway
    assert(!isSubtype[{ def f(): Unit }](new { def f(): Unit = () }))
  }


  val inferPrintlnEff = new { def muahaha = println() }
  test("println has effect") {
    assert(isSubtype[{ def muahaha: Unit @io }](inferPrintlnEff))
    assert(!isSubtype[{ def muahaha: Unit @noIo }](inferPrintlnEff))
  }

  // comment
  object T1 {
    def foo(): Int @noIo = {
//      println()
      1
    }
  }


  // this effect annotation doesn't make any sense - there should probably be a warning about it, but at least no error.
  val f: Int @noIo = 1


  // make sure refined types are inferred

  abstract class C {
    // allow any effect
    def foo(): Unit
  }
  type pureC = C { def foo(): Unit @noIo }

  test("infer refinement of class C") {
    val cPure = new C { def foo() = () }
    val cIo   = new C { def foo() = println() }

    assert(isSubtype[pureC](cPure))
    assert(!isSubtype[pureC](cIo))
    assert(isSubtype[C](cIo))
  }


  // functions

  type FunPure = (Int => Int) {
    def apply(x: Int): Int @noIo
  }

  test("infer effects of function trees") {
    val pureF = (x: Int) => x
    val ioF   = (x: Int) => { println(); x }

    assert(isSubtype[FunPure](pureF))
    assert(!isSubtype[FunPure](ioF))
    assert(isSubtype[Int => Int](ioF))
  }
}
