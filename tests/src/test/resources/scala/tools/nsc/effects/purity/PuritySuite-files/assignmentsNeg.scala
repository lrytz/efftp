import annotation.effects._

class C {
  def f1: C @loc() = this
  def f2: Int @loc() = 1
  def f3: String @loc() = "hai"

  def f8: Int @pure = {
    var x = 1
    def inc() { x = x + 1 }   // this is a bit unfortunate.. but procedures have an explicit Unit return type
    inc()                     // with no effect annotation - so in this case, `inc` gets the top effect
    x
  }

  // fails at calls to inc()
  def f8a: Int @pure = {
    var x = 1
    def inc(): Unit @assign(any) = { x = x + 1 }
    inc()
    x
  }


  // fails: the body of inc has effect @assign(x, any), not @assign(x, [fresh])
  def f9: Int @pure = {
    var x = 1
    def inc(): Unit @assign(x) = { x = x + 1 }
    inc(); inc();
    x
  }


  def f10a: Int @pure = {
    var y = 1
    def g: Unit @pure = {
      def h = { y = 2 }
      h
    }
    y
  }

  def f10: Int @pure = {
    var y = 1
    def g(): Int @loc(any) = {
      def h1() = {
        y = 2
      }
      def h2: Unit @loc(any) = {
        y = 4
      }
      def h3: Unit @assign(y, any) = {
        y = 6
      }
      h1()
      h2
      h3
      1
    }
    g()
  }
  
  
  def f11: Int @pure = {
    var x = 0
    def foo: Unit @loc(any) = "slfj" match {
      case s if {x = 1; true} => {x = 3}
    }
    10
  }
}
