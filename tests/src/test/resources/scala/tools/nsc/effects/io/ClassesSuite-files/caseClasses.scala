import annotation.effects.{io, pure}

object t {
  def t1: C @pure = C(1,2)(3).copy(y = 10)(4)
  def t2: String @pure = C(1,3)(4).toString
  def t3: Int @pure = C(1,3)(4).hashCode()
  def t4: Boolean @pure = C(1,2)(3) == C(2,3)(1)
}

case class C(x: Int, y: Int)(z: Int)
