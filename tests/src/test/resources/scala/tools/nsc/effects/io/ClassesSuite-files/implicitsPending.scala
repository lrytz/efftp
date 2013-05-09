import annotation.effects._
import scala.reflect.ClassTag

// ambiguous with plugin, but should chose cTwo

class C1 {
  class C

  trait A {
    implicit val cOne: C
  }

  abstract class B(implicit cTwo: C) extends A {
    def foo = implicitly[C]
  }
}

// type checks with plugin, but should be ambiguous

class C2a {
  class C
  def c: C @pure = new C

  trait A {
    implicit val cOne: C
  }

  abstract class B extends A {
    def foo = {
      implicit val cTwo = c
      implicitly[C]
    }
  }
}

// lazy val fixes it (i.e. we get an error now), because we attach effect annotations to lazy
// vals.. but that should maybe change as well.

class C2b {
  
  class C
  def c: C @pure = new C

  trait A {
    implicit val cOne: C
  }

  abstract class B extends A {
    def foo = {
      implicit lazy val cTwo = c
      implicitly[C]
    }
  }
}


// original code

class C3 {
  class UnrolledBuffer[T](implicit val tag: ClassTag[T])

  class DoublingUnrolledBuffer[T](implicit t: ClassTag[T]) extends UnrolledBuffer[T]()(t) {
    def newUnrolled = new Unrolled[T](0, new Array[T](4), null, this)
  }

  class Unrolled[T: ClassTag](var size: Int, var array: Array[T], var next: Unrolled[T], val buff: UnrolledBuffer[T] = null)
}