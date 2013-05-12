import annotation.effects._

class C {
  def no: Unit @noIo = ()
  def yo: Unit @io   = ()
  
  
  // pos ascription
  def f1 = {
    if (true) (no: @noIo) else (yo: @io)
    (no: @io)
  }
  
  // pos cast
  def f4: Unit @noIo = {
    (yo: @noIo @unchecked)
  }
  
  // pos cast
  def f5: Unit @noIo = {
    (yo: @unchecked @noIo)
  }
  
  // pos cast
  def f6: Unit @unchecked @noIo = {
    yo
  }
  
  // pos cast
  @unchecked @pure
  class C1 {
    println()
  }
  def t1: C1 @pure = new C1
  
  // pos cast
  class C2 {
    @unchecked @pure type constructorEffect
    println()
  }
  def t2: C2 @pure = new C2
  
  // pos cast
  class C3 {
    @unchecked @pure def this(x: Int) = {
      this()
      println()
    } 
  }
  def t3: C3 @pure = new C3(2)
  
  // pos cast
  class C4 {
    def this(x: Int) = {
      this()
      @unchecked @pure type constructorEffect = Nothing
      println()
    } 
  }
  def t4: C4 @pure = new C4(2)
}

class C1 {
  import scala.{unchecked => un}
  
  def no: Unit @noIo = ()
  def yo: Unit @io   = ()

  def f4: Unit @noIo = {
    (yo: @un @noIo)
  }
}

class C2 {
  import scala.{unchecked => !}
  
  def no: Unit @noIo = ()
  def yo: Unit @io   = ()

  def f4: Unit @noIo = {
    (yo: @`!` @noIo)
  }
}

class C3 {
  type un = scala.unchecked
  
  def no: Unit @noIo = ()
  def yo: Unit @io   = ()

  def f4: Unit @noIo = {
    (yo: @un @noIo)
  }
}
