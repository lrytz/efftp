import scala.annotation.effects._

object t {
  def t1: C1 @pure = new C1()
  def t2: C2 @pure = new C2()

  def t3: Cc.type @pure = Cc
  def t4: String @pure = Cc.toString
  def t5: Cc @pure = new Cc(10)
  def t6: Cc @pure = Cc(102)
  def t7: Int @pure = Cc.f
  def t8: Int @pure = Cc(12).x

  def t9: Trt @pure = new Trt {}

  def t10: Derive @pure = new Derive(27)
}

class C1 {
  def foo() { println() }
  val x = 1 + 1
  lazy val y = { println(); 2 }
}

trait Trt {
  val x = 1
  lazy val y = {println(); 2}
  def fai() { println() }
}

class C2 extends Trt


case class Cc(x: Int)

object Cc {
  val f = 10
}


class Super(x: Int)

@pure
class Derive extends { val x = 1 } with Super(10) {
  def this(x: Int) {
    this()
  }
}

