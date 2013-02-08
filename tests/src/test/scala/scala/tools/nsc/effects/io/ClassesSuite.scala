package scala.tools.nsc.effects.io

import org.scalatest.FunSuite
import scala.tools.nsc.effects.testing.TestMacros._

import scala.annotation.effects._

class ClassesSuite extends FunSuite {

  /*
  // effect casts: using ascription

  def foo(xs: Int*): Int @pure = {
    val x = 1
    (x /: xs)(_ + _): @pure
  }
  */

  /*
  // check overrides

  class C {
    def foo(): Int @pure = 1
  }

  class D extends C {
    override def foo(): Int @io = 2
  }

  */

  /*

  // effect relative to `this`

  class C {
    // top effect
    def bap(): Unit = ()

    def likeBap(): Unit @rel(this.bap()) = {
      this.bap()
    }
  }

  class D extends C {
    override def bap(): Unit @pure = ()
  }

  def lkdsjfl: Unit @pure = (new D).likeBap()
//  def lkdsdfl: Unit @pure = (new C).likeBap()

  */


/*
OK now, only one error message
  def twice(f: Int => Int, x: Int): Int @rel(f.apply(%)) = f(f(x))
  def plusTwoE(x: Int): Int @pure = twice(x => { println(); x + 1 }, x)
*/

/*
  def foo(f: Int => Int) = new {
    def bar(x: Int): Int @rel(f.apply(%)) = f(x)
  }
  // not yet supported.. has top effect
  def lkdsjflk: Any @pure = foo(x => x).bar(10)
*/


  /*
  class C(x: => Int) {
    @rel(x) type constructorEffect
//    @pure type constructorEffect

    def foos = x
    lazy val y = x
//    y
  }

  // not yet supported.. has top effect.
  def sldkfj: Int @pure = new C(10).foos
*/


/*
  def f00(x: => Int): Int @pure @rel(x) = {
//    x
    x
    1
  }

//  def sokdjf: Int @pure = f00({println(); 1})
  def sokdjf: Int @pure = f00({ 1 + 1 })
*/
/*

  @pure
  class LazyFals {
    lazy val x = { println(); 1 }
//    lazy val y: Int @pure = { println(); 1 }
  }

//  def foo: Unit @pure = new LazyFals().x




//  @pure
  object t {
//    println()
    def foo() = 1
  }

  def skldffoo: Int @pure = t.foo()


//  def f(): Unit @pure = { println() }

  @pure
  trait T {
    val x = 1
//    println()
  }

//  def f: T @pure = new T {}

  @pure
  class C extends T {
    val y = 2
//    println()
  }

  def gg: C @pure = new C




  class Super(x: Int) {
    //    println()
  }

  @pure
  class Derive extends { val x = 1 } with Super(10) {
    def this(x: Int) {
      this()
//      println()
    }
  }

  def seja: Derive @pure = new Derive(27)

*/


/*
  class K {
    val a = 3
    // error has additional message "type error occured during effect inference"
//    val y: K = ""

    // illegal cyclic reference, reported error says "need to annotate constructor effect"
//    val x: K = new K()

    // secondary constructor *always* needs effect annotation: the self constructor invocation always
    // goes through overloading resolution which forces the type of each alternative - including the
    // one that we're currently type checking.
    def this(x: Int) {
      this()
//      println()
    }

    // reported error says "constructor needs effect annotation"
//    val z = new K()

  }

  def mK: K @pure = new K()
  def mk2: K @pure = new K(1)
*/





  /*
  case class Cc(x: Int) {
    // un-commenting makes constructor impure
//    println()
  }
  object Cc {
    println()
    val f = 10
  }

  // todo: test copy method's effect

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
  */
}
