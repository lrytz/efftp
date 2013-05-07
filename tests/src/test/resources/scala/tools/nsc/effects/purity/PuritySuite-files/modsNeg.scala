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

  def f4(d: D, e1: E, e2: E): Unit @mod() = {
    var ei = e1
    d.eLoc = ei
    ei = e2
  }

  // whenever a clas has a parameter, the constructor effect is `@mod(this)` becuase
  // the fields for the parameters are initialized in the constructor
  class NoSuchElem(msg: String) extends Exception {
    @pure @mod() @loc() type constructorEffect
  }
  
  // `@local` parameters are always in the `@mod` effect of constructors
  class Ku(@local a: NoSuchElem) {
    @pure @mod() @loc() type constructorEffect
  }
}
