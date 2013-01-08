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

  type FunIo = (Int => Int) {
    def apply(x: Int): Int @io
  }

  val pureF = (x: Int) => x
  val ioF   = (x: Int) => { println(); x }

  test("infer effects of function trees") {
    assert(isSubtype[FunPure](pureF))
    assert(!isSubtype[FunPure](ioF))
    assert(isSubtype[Int => Int](ioF))
  }

  def cnd = math.random > 0.5

  // lubs

  test("lub via elimSub") {
    // this doesn't actually go through "annotationsLub", because one of the types is a subtype of the other,
    // so "def lub" in types.scala keeps the supertype by using "elimSub"
    val ioElimSub = if (cnd) pureF else ioF
    assert(!isSubtype[FunPure](ioElimSub))
    assert(isSubtype[FunIo](ioElimSub))

    val pureElimSub = if (cnd) (x: Int) => x else pureF
    assert(isSubtype[FunPure](pureElimSub))
    assert(isSubtype[FunIo](pureElimSub))
  }

  type PureF = { def f: Object @noIo }
  type IoF   = { def f: Object @io }

  test("lub doesn't go into refinement members") {
    class A; class B

    val objLub = if (cnd) new { def f: A = null } else new { def f: B = null }
    assert(!isSubtype[IoF](objLub))
    assert(isSubtype[Object](objLub))

    val objLub2 = if (cnd) new { def f: A @io = null } else new { def f: B @noIo = null }
    assert(!isSubtype[IoF](objLub2))
    assert(isSubtype[Object](objLub2))
  }

  // TODO: can only check that something DOES type check, but not that something doesn't, e.g. cannot check error message in
  //   val ptOrLub2: { def f: Object @noIo } = if (cnd) new { def f: A @io = null } else new { def f: B @noIo = null }
  test("refinements are kept & checked if they are in the expected type") {
    class A; class B

    val ptOrLub1: { def f: Object @noIo } = if (cnd) new { def f: A @noIo = null } else new { def f: B @noIo = null }
    assert(isSubtype[PureF](ptOrLub1))

    val ptOrLub2: { def f: Object @io } = if (cnd) new { def f: A @io = null } else new { def f: B @noIo = null }
    assert(!isSubtype[PureF](ptOrLub2))
    assert(isSubtype[IoF](ptOrLub2))

    // the test below doesn't work as expected because there's no contravariant subtyping in scala for
    // method parameters, see next test. So the expected type with the refinement demands an apply method that takes
    // a parameter of type `String`. The apply mehtod of `(o: Object) => o` does therefore not conform.]

    /*
    type SOFun = (String => Object) {
      def apply(s: String): Object @noIo
    }

    val fun: SOFun = if (cnd) (s: String) => s else (o: Object) => o
    assert(isSubtype[SOFun](fun))
    assert(isSubtype[String => Object](fun))
    */
  }

  // see https://groups.google.com/forum/#!topic/scala-internals/kSJLzYkmif0/discussion
  test("scala doesn't have contra-variant subtyping for method parameters") {
    val foo = new { def f(o: Object) = o }
    assert(!isSubtype[{ def f(s: String): Object }](foo))
  }

}
