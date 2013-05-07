import annotation.effects._

class C {
  
  class E1 extends Exception
  class E2 extends Exception
  
  def f(): Unit @pure = ()
  
  def g(): Unit @pure @io = ()
  
  def h(): Unit @pure @throws[E1] = ()
  
  def i(): Unit @pure @mod(any) = ()

  
  def f1: Unit @pure = {
    f()
    try {
      h()
    } catch {
      case _: E1 => f()
    }
  }

}