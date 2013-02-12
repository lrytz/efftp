import scala.annotation.effects._

object t {
  def t1: C1 @pure = new C1()
  def t2: C2 @pure = new C2()
}

class C1 {
  println()
}

class C2 {
  val x = { println(); 1 }
}
