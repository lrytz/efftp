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
    @local val y: E
    @local val z: D
  }

  def f9a(d: D): E @loc() = d.x
  def f9b(d: D): E @loc(d) = d.x

  def f10(d: D): D @loc() = {
    val d1 = d
    d1
  }

  def f11(d1: D, d2: D): D @loc() = {
    var dr = d1
    dr = d2
    dr
  }

  def f12(d: D): E @loc() = d.y
  def f13(d: D): Any @loc() = if (true) d else d.y
  def f14(d: D): E @loc() = if (true) d.y else if (false) d.z.y else d.z.z.y
}
