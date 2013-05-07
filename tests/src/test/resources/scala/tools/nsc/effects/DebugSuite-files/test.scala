import annotation.effects._

class C {

  // TODO: fails, but should not
  class C(x: String = "aha")
  def f: C @pure @loc() = new C()


  // TODO: why is @mod(this) necessary??? fails without. fix also in colls.scala
  class NoSuchElem(msg: String) extends Exception {
    @pure @mod(this) @loc() type constructorEffect
  }

}
