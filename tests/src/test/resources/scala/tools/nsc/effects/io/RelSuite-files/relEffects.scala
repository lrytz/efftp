import annotation.effects._

object t {
  def t1: Unit @pure = (new D).likeBap()
  def t2: C1 @pure = new C1(1)

  def t3: Int @pure = foo(x => x + 1)
  def t4: Int @pure = bar(new MApp())

  def foo(f: Int => Int): Int @pure(f) = f(f(0))
  def bar(m: MApp): Int @pure(m) = m.apply(10) + m.apply(10,20)
  
  def t5(x: => Int): Int @pure(x) = {
    if (true) 10
    else x
  }

}

// effects relative to this
class C {
  // top effect
  def bap(): Unit = ()

  def likeBap(): Unit @pure(this.bap()) = {
    this.bap()
  }
}

class D extends C {
  override def bap(): Unit @pure = ()
}

class C1(x: => Int) {
  @pure(x) type constructorEffect
  val uuh = x
  def foos() = x   // has effect
  lazy val y = x   // has effect.. see relEffectsNeg
}


class MApp {
  def apply(x: Int): Int @pure = 0
  def apply(x: Int, y: Int): Int @pure = 1
}

