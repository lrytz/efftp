package scala.tools.nsc.effects.io

import org.scalatest.FunSuite
import scala.tools.nsc.effects.testing.TestMacros._

import scala.annotation.effects._

class ClassesSuite extends FunSuite {

  case class Cc(x: Int) {
    // un-commenting makes constructor impure
//    println()
  }
  object Cc {
    println()
    val f = 10
  }

  def compObj: Cc.type @pure = Cc

  def toStr: String @pure = Cc.toString

  def mkC: Cc @pure = Cc(1)

  def getX: Int @pure = mkC.x

  def testF: Int @pure = Cc.f

  class C {
    var x = 1
  }

  def inc(i: Int) = i + 1

  def computeInt() = {println(); 102}


  class A {
    val field = inc(10)

    var vf = 102

    def incr(): Unit @pure = { vf = vf + 10 }

    // makes the constructor non-pure
//    val fold = computeInt()

    // also makes constr impure
//    println()

    def readField = field
  }

  def makeA: A @pure = new A
  def testA: Int @pure = makeA.field

  def testA1: Unit @pure = makeA.incr()

//  test("constructor effects are inferred") {
//    val t1 = new { def makeA = new A }
//    assert(isSubtype[{def makeA: A @pure}](t1))
//  }
}
