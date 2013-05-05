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
  
  
  def f1(d: D): Unit @mod(d) = {
    d.i = 1
  }
  
  def f2(d: D, e: E): Unit @mod(d) = {
    d.eGlob = e
  }

  def f3(d: D, e: E): Unit @mod(d, e) = {
    d.eLoc = e
  }
  
  def f4(d: D, e1: E, e2: E): Unit @mod(d, e1, e2) = {
    var ei = e1
    d.eLoc = ei
    ei = e2
  }

}
