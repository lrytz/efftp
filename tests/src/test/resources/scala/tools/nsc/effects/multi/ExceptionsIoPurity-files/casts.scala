import annotation.effects.{io, pure}

class C {
  def show(a: Any): Unit @pure @io = {
    // need an additional cast because the argument is ANF-lifted to a local variable,
    // and it ends up outside the other ascription.
    println(a.toString: @pure): @pure @io
  }
}
