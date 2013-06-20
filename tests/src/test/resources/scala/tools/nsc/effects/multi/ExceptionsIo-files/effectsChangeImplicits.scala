import annotation.effects._

class C {
  def doApply(implicit f: { def m(): Object @pure }) = null

  class A {
    implicit val m: { def m(): String } = null
  }

  class B extends A {
    implicit val n: { def m(): Object @pure } = null

    doApply
  }
}
