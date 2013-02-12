import annotation.effects.{io, pure}

class C {
  def foo(): Int @pure = 1
  def bar(): Int @pure = 1
}

class D extends C {
  override def foo(): Int @io = 2
  override def bar() = { println(); 2 }
}
