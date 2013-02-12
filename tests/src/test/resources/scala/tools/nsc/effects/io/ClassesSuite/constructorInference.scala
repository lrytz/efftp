import scala.annotation.effects._

class C {
  def foo() { println() }
  val x = 1 + 1
  lazy val y = { println(); 2 }

  def make: C @pure = new C()
}
