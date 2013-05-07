import annotation.effects.{io, noIo}

class C {
  // former bug: java.lang.Long has NoSymbol (after type checking)!
  def foo: Int @io = java.lang.Long.bitCount(10l)
}
