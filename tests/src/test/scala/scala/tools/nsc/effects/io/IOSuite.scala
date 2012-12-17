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
    assert(isSubtype[{ def f(): Unit @noIo }](withIO))
    assert(!isSubtype[{ def f(): Unit @io }](withoutIO))
    assert(isSubtype[{ def f(): Unit @noIo }](withoutIO))
  }

  test("type inference does not keep refinements when expected type is any") {
    // due to some special case with anonymous classes and refinement types, see
    // typedBlock in Typers.scala
    // this makes the inferred type of "new { ...}" only "Object{}", the refined
    // meber is dropped because the expected type is Any, so it would not be
    // accessible anyway
    assert(!isSubtype[{ def f(): Unit }](new { def f(): Unit = () }))
  }


  val inferPrintlnEff = new { def muahaha = println() }
  test("println has effect") {
    // comments
    assert(isSubtype[{ def muahaha: Unit @io }](inferPrintlnEff))
  }
}
