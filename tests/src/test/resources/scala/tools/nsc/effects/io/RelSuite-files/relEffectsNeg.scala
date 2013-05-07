import annotation.effects._

object t {
  def t1: Unit @pure = (new C).likeBap()

  // maake sure we only get one error message here
  def twice(f: Int => Int, x: Int): Int @rel(f) = f(f(x))
  def plusTwoE(x: Int): Int @pure = twice(x => { println(); x + 1 }, x)



  def foo(f: Int => Int) = new {
    def bar(x: Int): Int @rel(f.apply(%)) = f(x)
  }

  // not yet supported.. has top effect
  def t2: Any @pure = foo(x => x).bar(10)


  // not yet supported.. has top effect.
  def t3: Int @pure = new C1(10).foos
  def t4: Int @pure = new C1(10).y

  def t5(x: => Int): Int @pure = {
    if (true) 10
    else x
  }
}

// effects relative to this
class C {
  // top effect
  def bap(): Unit = ()

  def likeBap(): Unit @rel(this.bap()) = {
    this.bap()
  }
}

class C1(x: => Int) {
  @pure type constructorEffect
  def foos = x   // has effect! see below
  lazy val y = x // @rel(x) would not work here; x is a field. also if it has type @rel(x), that effect would need
                 // to be replaced with a concrete effect on instantiating a C, which is not yet supported.
  y // has effect!
}

abstract class A {
  def foo: Int
  def faa: Int = 1
}

object t5 {
  def hm(a: A): Int @noIo @rel(a.foo) = {
    a.foo
  }
  def hm0(a1: A { def foo: Int @noIo }): Int @noIo = {
    hm(a1)
  }
  val aIo   = new A { def foo = {println(); 2} }
  hm0(aIo)
}


object t6 {
  def needPure(f: (Int => Int){ def apply(x: Int): Int @noIo }): Int @noIo = f(1)
  def hoRel(g: Int => Int): Int @noIo @rel(g.apply(%)) = {
    val funRel = (x: Int) => g(x)
    // this should NOT type check (and doesn't): the function `funRel` has a relative effect and
    // therefore doesn't conform to the expected type of `needPure`
    needPure(funRel)
  }
}

object t7 {
  def ho(f: Int => Int): Int @rel(f.apply(%)) = {
    def t12: (() => Int) {def apply(): Int @noIo} = f1
    0
  }
}


object t8 {
  def hm(a: A): Int @io @rel(a.foo, a.faa) = {
    val f1 = () => a.foo
    def tf2: (() => Int) {def apply(): Int @rel(a.foo)} = f1
    def tf3: (() => Int) {def apply(): Int @rel()} = f1

    val o1 = new { def t: Int @rel(a.foo) = a.foo }
    def to3: { def t: Int @rel(a.faa) } = o1
    def to4: { def t: Int @rel() } = o1

    0
  }

}

// `hm` doesn't exist. used to crash the plugin
object t9 {
  abstract class A { def foo: Int }
  val block1 = {
    val aPure = new A { def foo = 1 }
    def tm1: Int @noIo = hm(aPure)
  }
}

// also used to crash, after reporting the (correct) error
object t10 {
  abstract class A { def foo: Int }
  def hn(a: A):Int @noIo @rel(a.foo) = {
    def buz: Int @noIo = a.foo // not allowed, a.foo might have an effect.
    buz
  }
}
