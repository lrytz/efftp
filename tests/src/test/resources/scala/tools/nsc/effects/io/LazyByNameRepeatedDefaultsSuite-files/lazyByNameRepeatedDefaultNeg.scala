import scala.annotation.effects._

object t {
  def t1: Int @pure = new LazyFals().z
  def t2: Int @pure = new LazyFals().flam()
  def t3: Int @pure = f00R({println(); 1})


  def f00(x: => Int): Int @pure = {
    x
    1
  }
  def f00R(x: => Int): Int @pure(x) = x
}

@pure
class LazyFals {
  lazy val z = { println(); 1 }

  def flam() = {
    lazy val x = {println(); 1}
    10 + x
  }
}


object defaults {

  // defaults
  def fip(x: Int @pure = {println(); 1} ) = x
  def fap(x: Int = {println(); 1} ) = x
  def fup(x: Int = 1) = x

  def t1: Int @pure = fip()
  def t2: Int @pure = fap()
  def t3: Int @pure = fup()
}

object repeated {
  // repeated parameters
  def huh(x: Int, y: Int*): Int @pure = 1
  def t1: Int @pure = huh(1,2,3,{println();1})

  def hoh(x: Int, y: Int*)(f: Int => Int): Int @pure(f) = f(x)
  def t2: Int @pure = hoh(1,2,3)(x => {println(); x + 1})
}
