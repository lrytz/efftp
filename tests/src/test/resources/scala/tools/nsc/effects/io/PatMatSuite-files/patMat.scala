import annotation.effects._

trait Nums
case class One(x: Int) extends Nums
case class Two(x: Int, y: Int) extends Nums

object t {
  def foo(a: Nums): Int @pure = a match {
    case One(x) => x
    case Two(x, y) => x + y
  }

  def t1: Int @pure = foo(Two(1,2))

  def t2(a: Nums): Int @pure = a match {
    case One(x) if x == 10 => x
    case Two(x, y) if x - y == 0 => -1
    case One(_) | Two(_, 10) => -2
    case _: Two => 42
    case _ => 42
  }

  def t3(x: Int): Int @pure = x match {
    case Even() => x / 2
    case _ => x
  }
}

object Even {
  def unapply(x: Int): Boolean @pure =
    if (x % 2 == 0) true
    else false
}

// unapply with result value: need effect annotations or default effects for Option type