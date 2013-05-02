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
  
  
  // non-local field selection
  
  trait E
  trait D {
    val x: E
  }

  def f9(d: D): E @loc(any) = d.x
  def f10(d: D): D @loc(d) = {
    val d1 = d
    d1
  }
  
}
