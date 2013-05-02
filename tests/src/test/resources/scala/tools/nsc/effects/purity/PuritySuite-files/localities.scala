import annotation.effects._

class C {
  def f1: Int @loc(any) = 1
  def f2 = ""
  def f3: String @loc(any) = f2

  def f4: C @loc(this) = this
  def f5: C @loc(any) = this

  def f6(a: C): C @loc(a) = a

  def f7(a: C): C @loc(a, this) = if (true) a else this


  // pattern matching. localities of pattern-bound variables are not yet correctly computed, some
  // tests are therefore in localitiesPending
  
  def f8(a: Any): C @loc(any) = a match {
    case c: C => c
    case _ => this
  }
  
  trait E
  trait D {
    val x: E
    @local val y: E
    @local val z: D
  }

  
  // non-local field selection
  
  def f9(d: D): E @loc(any) = d.x
  def f10(d: D): D @loc(d) = {
    val d1 = d
    d1
  }
  def f11(d1: D, d2: D): D @loc(d1, d2) = {
    var dr = d1
    dr = d2
    dr
  }
  

  // local fields
  
  def f12(d: D): E @loc(d) = d.y
  def f13(d: D): Any @loc(d) = if (true) d else d.y
  def f14(d: D): E @loc(d) = if (true) d.y else if (false) d.z.y else d.z.z.y

  def f15(d: D): E @loc(d) = {
    val da = d.z.z
    def foo(): E @loc(da, d) = {
      if (true) da.y else d.y
    }
    foo()
  }

  def f16(d: D): D @loc(d) = {
    def foo() = d.z.z
    foo()
  }
  
  
  object obj extends E { val x = 1 }
  
  def f17: E @loc(any) = obj
  
  def f18(d: D): E @loc(d) = {
    var da = d.z
    var x = 0
    while(x < 10) {
      da = da.z
      da
    }
    da.y
  }

}

