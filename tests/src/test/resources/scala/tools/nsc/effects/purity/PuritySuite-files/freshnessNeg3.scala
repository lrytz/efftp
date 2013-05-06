import annotation.effects._

class C {
  class K

  class A13b {
    @local var c = new K
    def this(k: K) {
      this()
      c = k
    }
  }
  def f13ba(): A13b @loc() = new A13b
  def f13bb(): A13b @loc() = new A13b(new K)
  def f13bc(k: K): A13b @loc() = new A13b(k)
}
