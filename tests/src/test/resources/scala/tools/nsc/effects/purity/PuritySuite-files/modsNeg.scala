import annotation.effects._

class C {
  trait E
  trait D {
    var i: Int
    var eGlob: E
    @local var eLoc: E
    @local var d: D
  }

  object obj extends E
  
  
  def f1(d: D): Unit @mod() = {
    d.i = 1
  }
  
  def f2(d: D, e: E): Unit @mod() = {
    d.eGlob = e
  }

  def f3(d: D, e: E): Unit @mod() = {
    d.eLoc = e
  }
  
}
