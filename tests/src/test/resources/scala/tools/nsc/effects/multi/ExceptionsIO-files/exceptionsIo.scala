import annotation.effects._

class C {
  
  def foo: Unit @pure @io = {
    println()
  }
  
}
