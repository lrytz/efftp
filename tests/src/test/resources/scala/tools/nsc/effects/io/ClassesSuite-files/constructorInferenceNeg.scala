import scala.annotation.effects._

object t {
  def t1: C1 @pure = new C1()
  def t2: C2 @pure = new C2()
  def t3: C3 @pure = new C3()
  def t4: C4 @pure = new C4()
  def t5: C5 @pure = new C5(1)
  def t6: C6 @pure = new C6()
  def t7: C7 @pure = new C7(10)



  def compObj: Cc.type @pure = Cc
  def toStr: String @pure = Cc.toString
  def mkC: Cc @pure = Cc(1)
  def testF: Int @pure = Cc.f
  def ccConstr: Cc @pure = new Cc(10)
}

class C1 {
  println()
}

class C2 {
  val x = { println(); 1 }
}

class C3 extends { val x = { println(); 1 } } with AnyRef {}

class C4 {
  def foo() = {println(); 1}
  foo()
}

class C5(x: => Int) {
  val y = x
}

trait Trt { println() }

class C6 extends Trt

class C7 {
  def this(x: Int) = {
    this()
    println()
  }
}


case class Cc(x: Int) {
  println()
}

object Cc {
  println()
  val f = 10
}


class Flupi {
  // there was a bug which prevented this error at some point, object selections can be Ident trees
  def foo: Int @pure = tee.x
}

object tee {
  println()
  val x: Int = 1
}



