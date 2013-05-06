import annotation.effects._

class C {
  @loc(any)
  class A2 {
    // no error here; constructors are always inferred as fresh, even if they call another
    // constructor which is not fresh. should be fixed
    @loc()
    def this(x: Int) = this()
  }
  
  def t14: Object @loc() = new Object()  
}
