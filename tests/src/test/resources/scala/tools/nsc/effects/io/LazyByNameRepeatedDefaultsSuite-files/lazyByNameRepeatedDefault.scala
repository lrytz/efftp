import scala.annotation.effects._

object t {
  def t1: Int @pure = new LazyFals().flam()
  def t2: Int @pure = f00(1)
  def t3: Int @pure = f00({ 1 + 1 })

  def f00(x: => Int): Int @rel(x) = {
    x
    1
  }
}

@pure
class LazyFals {
  lazy val x = { println(); 1 }

  def flam() = {
    lazy val x = {println(); 1}
    10 + 1
  }
}


object defaults {

  // defaults
  def fip(x: Int @pure = 1 ) = x

  def t1: Int @pure = fip()
  def t2: Int @pure = new DefClass().y
}

class DefClass(x: Int @pure = 1) {
  def y = x
}


object repeated {
  // repeated parameters
  def huh(x: Int, y: Int*): Int @pure = 1

  def t1: Int @pure = huh(1,2,3,{1+1})

  def hoh(x: Int, y: Int*)(f: Int => Int): Int @rel(f) = f(x)

  def t2: Int @pure = hoh(1,2,3)(x => x + 1)

  def hih(g: Int => Int): Int @rel(g) = hoh(1)(g)
}