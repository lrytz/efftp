import annotation.effects._

class C {
  
  def f(): Unit @pure @io = () 

  def g(): Unit @pure @throws[java.io.IOException] = ()
  
  def h(): Unit @pure @io @throws[java.io.IOException | java.lang.InterruptedException] = ()

  def f1: Unit @pure = {
    h()
  }

  def f2: Unit @pure @io = {
    // two error messages: selection of "Predef" (object initializer) and the println() method
    println()
  }

  def f3: Unit @pure = {
    f()
    f()
  }

  def f4: Unit @pure = {
    f()
    g()
  }

  def f5: Unit @pure = {
    f()
  }

  class E1 extends Exception
  class E2 extends Exception

  def f6: Int @pure @throws[Nothing] = {
    f1
    throw new E1
  }
}
