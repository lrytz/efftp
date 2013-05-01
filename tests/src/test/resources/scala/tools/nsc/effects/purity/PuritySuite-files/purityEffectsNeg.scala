import annotation.effects._

class C {
  def f1: C @loc() = this
  def f2: Int @loc() = 1
  def f3: String @loc() = "hai"
}
