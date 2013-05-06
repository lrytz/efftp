import annotation.effects._

class C {
  trait D

  class A13Trait {
    @local var c = new D {}
    def this(k: D) {
      this()
      c = k
    }
  }
  def f13Ta(): A13Trait @loc() = new A13Trait
  def f13Tb(): A13Trait @loc() = new A13Trait(new D {})
  def f13Tc(d: D): A13Trait @loc() = new A13Trait(d)
}
