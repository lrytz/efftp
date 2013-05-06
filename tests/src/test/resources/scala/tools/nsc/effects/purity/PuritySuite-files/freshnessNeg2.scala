import annotation.effects._

class C {


  class K

  class A13a {
    @mod() @loc() type constructorEffect
    @local var c = new K
    @mod() @loc() def this(k: K) {
      this()
      c = k
    }
  }
}
