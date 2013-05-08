import scala.annotation.effects._

object t {
  def t1: C1 @pure = new C1()
  def t2: C2 @pure = new C2()

  def t3: Cc.type @pure = Cc
  def t4: String @pure = Cc.toString
  def t5: Cc @pure = new Cc(10)
  def t6: Cc @pure = Cc(102)
  def t7: Int @pure = Cc.f
  def t8: Int @pure = Cc(12).x

  def t9: Trt @pure = new Trt {}

  def t10: Derive @pure = new Derive(27)
}

class C1 {
  def foo() { println() }
  val x = 1 + 1
  lazy val y = { println(); 2 }
}

trait Trt {
  val x = 1
  lazy val y = {println(); 2}
  def fai() { println() }
}

class C2 extends Trt


case class Cc(x: Int)

object Cc {
  val f = 10
}


class Super(x: Int)

@pure
class Derive extends { val x = 1 } with Super(10) {
  def this(x: Int) {
    this()
  }
}




class C3 {
  def this(i: Int) = {
    this()
  }
  
  def foo() = {
    new C3(10)
  }
}



class C4 {
  def this(i: Int) = {
    this()
    foo()
  }
  
  def foo(): C4 @pure = {
    new C4(10)
  }
}

class C5 {
  class H[T]
  def d[T] = new H[T]

  class T[K] {
    def this(h: H[K]) = this()
    def this(x: Int, y: Int) = this(d)
  }
}



class C7 {
  def this(a: String) = this()
  def this(b: Int) = this()
  def this(a: Int, b: Int) = this(a + b)
}



trait H[T]
object H {
  class D[T] extends H[T]
  def d[T] = new D[T]
}
trait E[T]
object E {
  def u[T]: E[T] = new E[T] {}
}

class T[K, V](x: Int) {
  def this(h: H[K], e: E[K]) = this(1)
  def this() = this(H.d, E.u)
}



// imports within a template
class M1 {
  import M1._
  protected var x: Int = foo()
}

object M1 {
  def foo() = 1
}



