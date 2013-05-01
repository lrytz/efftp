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


}
