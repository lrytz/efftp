import annotation.effects._

class C {
  def f(): Unit @pure @io = () 

  def g(): Unit @pure @throws[java.io.IOException] = ()
  
  def h(): Unit @pure @io @throws[java.io.IOException | java.lang.InterruptedException] = ()

  def f1: Unit @pure = ()
  
  def f2: Unit @io @throws[Exception] @pure = ()

  def f3: Unit @io @throws[Exception] @pure = h()

  def f4: Unit @io @throws[Exception] @pure = {
    f()
    g()
  }
  
  def f5: Int @pure @io = {
    f()
    1
  }
  
  class E1 extends Exception
  class E2 extends Exception

  def f6: Int @pure @throws[E1 | E2] = {
    f1
    throw new E1
  }
}
