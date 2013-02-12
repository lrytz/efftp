import annotation.effects.pure

class C {
  def foo(): Int @pure = 1
}

class D extends C {
  override def foo(): Int @pure = 2
}
