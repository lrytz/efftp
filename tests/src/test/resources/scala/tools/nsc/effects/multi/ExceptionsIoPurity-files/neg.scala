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
      case _: E2 => i()
    }
  }

  def f2: Unit @pure = {
    f()
    h()
    i()
  }
  
  def f3: Unit @pure = {
    f()
    g()
    i()
  }
  
  def f5: Unit @pure = {
    f()
    g()
    h()
  }

}