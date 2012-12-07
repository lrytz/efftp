package scala.tools.nsc.effects.io

import org.scalatest.FunSuite
import scala.tools.nsc.effects.testing.TestMacros._

import scala.annotation.effects._

class IOSuite extends FunSuite {
  def foo(): Unit @io = { println() }

  val withIO = new { def f() = foo() }

  test("method with io") {
    assert(isSubtype[{ def f(): Unit @io }](withIO))
    assert(isSubtype[{ def f(): Unit @noIo }](withIO))
  }
}
