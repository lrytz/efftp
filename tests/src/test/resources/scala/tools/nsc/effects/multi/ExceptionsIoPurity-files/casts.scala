import annotation.effects.{io, pure}

class C {
  class A {
    def s: String = ""
  }

  def show(a: A): Unit @pure @io = {
    // need an additional cast because the argument is ANF-lifted to a local variable,
    // and it ends up outside the other ascription.
    println(a.s: @unchecked @pure): @unchecked @pure @io
  }
}
