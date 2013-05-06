import annotation.effects._

class C {
  class A1
  def f1(): A1 @loc() = {
    new A1
  }
  
  class A2 {
    def this(x: Int) = this()
  }
  def f2a: A2 @loc() = {
    new A2()
  }
  def f2b: A2 @loc() = {
    new A2(101)
  }
  
  class A3 {
    var x: Int = 0
    def this(y: Int) {
      this()
      x = y
    }
  }
  def f3a: A3 @loc() = {
    new A3()
  }
  def f3b: A3 @loc() = {
    new A3(102)
  }


  class A4 {
    @loc() type constructorEffect
  }
  
  @loc()
  class A5

  
  class A6 {
    @loc() def this(x: Int) = this()
  }
  
  class A7 {
    @mod(this) @loc() type constructorEffect
    var x = 0
    x = 1
  }

  class A8 {
    @mod() @loc() type constructorEffect
    var x = 0
  }


  class A9 {
    var x = 0
    @mod(this) @loc() def this(x: Int) = {
      this()
      this.x = x
    }
  }
  
}
