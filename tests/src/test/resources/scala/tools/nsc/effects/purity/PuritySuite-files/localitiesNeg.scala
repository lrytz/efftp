import annotation.effects._

class C {
  def f1: Int @loc(this) = 1
  def f2 = ""
  def f3: String @loc(this) = f2
  def f4: String @loc() = f2

  def f5: C @loc() = this

  def f6a(a: C): C @loc(this) = a
  def f6b(a: C): C @loc(a) = this

  def f7a(a: C): C @loc(a) = if (true) a else this
  def f7b(a: C): C @loc(this) = if (true) a else this
  def f7c(a: C): C @loc() = if (true) a else this

  trait E
  trait D {
    val x: E
  }

  def f9a(d: D): E @loc() = d.x
  def f9b(d: D): E @loc(d) = d.x

  def f10(d: D): D @loc() = {
    val d1 = d
    d1
  }
}
