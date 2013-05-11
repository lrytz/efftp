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
  
  
  def f7: Unit @pure = {
    (f(): @pure)
  }

  def f8a: Unit @pure @throws[E1] = {
    {
      throw new E1
      f()
    }: @noIo @throws[E1]
  }

  def f8b: Unit @pure @throws[E1] = {
    {
      throw new E1
      f()
    }: Unit @noIo @throws[E1]
  }

  def f9a[B](default: () => B): B @pure(default.apply()) = 
    default()

  def f9aa[B](default: () => B): B @pure(default) = 
    default()

  def f9b(default: => Int): Int @pure(default) =
    default

  def f9b[B](default: => B): B @pure(default) = 
    default
}
