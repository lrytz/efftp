import annotation.effects._

class C {
  @loc(any)
  class A1
  def f1(): A1 @loc() = {
    new A1
  }
  
  @loc(any)
  class A2 {
    // no error here; constructors are always inferred as fresh, even if they call another
    // constructor which is not fresh. should be fixed
    @loc()
    def this(x: Int) = this()
  }
  def f2a: A2 @loc() = {
    new A2()
  }
  def f2b: A2 @loc() = {
    new A2(101)
  }
  
  @loc()
  class A3 {
    var x: Int = 0
    @loc()
    def this(y: Int) {
      this()
      x = y
    }
  }

  class A4 {
    @loc(any) type constructorEffect
  }
  def f4: A4 @loc() = new A4
  

  class A7 {
    @mod() @loc() type constructorEffect
    var x = 0
    x = 1
  }

  class A8 {
    @mod() @loc() type constructorEffect
    var x = 0
  }


  @mod() @loc()
  class A9 {
    var x = 0
    @mod() @loc() def this(x: Int) = {
      this()
      this.x = x
    }
  }
  
}
