import annotation.effects._

trait Nums
case class One(x: Int) extends Nums
case class Two(x: Int, y: Int) extends Nums

object t {
  def t2(a: Nums): Int @pure = a match {
    case One(x) if {println(); x == 10} => x
    case _: Two => 42
  }

  def t3(x: Int): Int @pure = x match {
    case Even() => x / 2
    case _ => x
  }

}


object Even {
  def unapply(x: Int) =
    if (x % 2 == 0) true
    else {println(); false}
}
