package scala.tools.nsc.effects.io

import org.scalatest.FunSuite
import scala.tools.nsc.effects.testing.TestMacros._

import scala.annotation.effects._

class RelSuite extends FunSuite {

  def ho(f: Int => Int): Int @noIo @rel(f.apply(%)) = {
    f(10)
  }

  test("ho is effect polymorphic") {
    val t1 = new { def t = ho(x => x) }
    val t2 = new { def t = ho(x => {println(); x}) }

    assert( isSubtype[{def t: Int @noIo}](t1))
    assert(!isSubtype[{def t: Int @noIo}](t2))
    assert( isSubtype[{def t: Int @io  }](t2))
  }


  abstract class A {
    def foo: Int
    def faa: Int = 1
  }

  def hm(a: A): Int @noIo @rel(a.foo) = {
    a.foo
  }

  test("relative effect with non-function parameter") {
    val aPure = new A { def foo = 1 }
    val aIo   = new A { def foo = {println(); 2} }

    val t1 = new { def t = hm(aPure) }
    val t2 = new { def t = hm(aIo) }

    assert( isSubtype[{def t: Int @noIo}](t1))
    assert(!isSubtype[{def t: Int @noIo}](t2))
    assert( isSubtype[{def t: Int @io  }](t2))
  }

  test("method with pure a as parameter can call polymorphic method") {
    def hm0(a1: A { def foo: Int @noIo }): Int @noIo = {
      hm(a1)
    }

    val aPure = new A { def foo = 1 }
    val aIo   = new A { def foo = {println(); 2} }

    hm0(aPure)
    // @TODO: neg test
    // hm0(aIo)
  }

  test("forward parameter of effect-poly method into another effect-poly method") {
    def hm1(a1: A): Int @noIo @rel(a1.foo) = {
      hm(a1)
    }
  }


  test("method / function definitions inside an effect-polymorphic method inheirt the rel effect") {
    def hn(a: A): Int @noIo @rel(a.foo) = {
      // should test that inferred type has the @rel(a.foo) annotation (checked in debugger, it's ok)
      // just inferring @noIo would be unsound
      def bar = a.foo
      def buz: Int @noIo @rel(a.foo) = a.foo
      val fun = () => a.foo
      val fon: (() => Int) { def apply(): Int @noIo @rel(a.foo) } = () => a.foo
      bar
      buz
      fun()
      fon()
    }
  }


  test("relative effects are translated between effect-polymorphic functions") {
    def ho2(g: Int => Int): Int @noIo @rel(g.apply(%)) = {
      // inferred effect for "x => g(x)" is (Int => Int @noIo @rel(g.apply(%)) - not just @noIo, that would
      // be unsound. see neg test below.
      ho(x => g(x))
    }
  }

  def needPure(f: (Int => Int){ def apply(x: Int): Int @noIo }): Int @noIo = f(1)

  test("function literals have relative effect annotation, but only if necessary") {
    def hoRel(g: Int => Int): Int @noIo @rel(g.apply(%)) = {
      val funRel = (x: Int) => g(x)
      // @TODO: neg test
      // this should NOT type check (and doesn't): the function `funRel` has a relative effect and
      // therefore doesn't conform to the expected type of `needPure`
      // needPure(funRel)

      // for funP, the inference algorithm does NOT add a relative effect `@rel(g.apply)` because
      // the body of the function does not call the method `g.apply`
      val funP = (x: Int) => x
      needPure(funP)
    }
  }


  test("passing parameter as argument into another effect-polymorphic method translates the relative effect") {
    def ho3(g: Int => Int): Int @noIo @rel(g.apply(%)) = {
      ho(g)
    }
  }


  test("subtyping and relative effects") {
    def ho(f: Int => Int): Int @rel(f.apply(%)) = {
      val f1 = () => f(10)

      def t11: (() => Int) {def apply(): Int @rel(f.apply(%))} = f1
      // @TODO: neg test
      // this should not compile
      // def t12: (() => Int) {def apply(): Int @noIo} = f1
      def t13: (() => Int) {def apply(): Int @io} = f1

      val f2 = () => 10
      def t21: (() => Int) {def apply(): Int @rel(f.apply(%))} = f2
      def t22: (() => Int) {def apply(): Int @pure} = f2
      def t23: (() => Int) {def apply(): Int @io} = f2

      0
    }

    def hm(a: A): Int @io @rel(a.foo, a.faa) = {
      val f1 = () => a.foo

      def tf1: (() => Int) {def apply(): Int @rel(a.foo, a.faa)} = f1
      // @TODO: neg test
      // the first could compile in theory, but inference keeps either all or no relative effects for functions
      // def tf2: (() => Int) {def apply(): Int @rel(a.foo)} = f1
      // def tf3: (() => Int) {def apply(): Int @rel()} = f1

      val o1 = new { def t: Int @rel(a.foo) = a.foo }

      def to1: { def t: Int @rel(a.foo, a.faa) } = o1
      def to2: { def t: Int @rel(a.foo) } = o1
      // @TODO: neg test
      // def to3: { def t: Int @rel(a.faa) } = o1
      // def to4: { def t: Int @rel() } = o1
      def to5: { def t: Int } = o1

      0
    }
  }


  /*
  @TODO: neg test
  The following used to crash the plugin (because the "hm(aPure)" tree is erroneous, hm doesn't exist, but
  the plugin still tried to compute an effect -> NoSym has no owner at some point

  abstract class A { def foo: Int }
  val block1 = {
    val aPure = new A { def foo = 1 }
    def tm1: Int @noIo = hm(aPure)
  }
  */




  /*
  @TODO: neg test
  The following used to crash the plugin (after reporting the effect error), so should write a neg test for it
    abstract class A { def foo: Int }
    def hn(a: A):Int @noIo @rel(a.foo) = {
      def buz: Int @noIo = a.foo
      buz
    }
   */
}
