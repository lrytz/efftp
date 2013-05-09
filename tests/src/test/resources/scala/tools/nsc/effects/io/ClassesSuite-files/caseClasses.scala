import annotation.effects.{io, pure}

object t {
  def t1: C @pure = C(1,2)(3).copy(y = 10)(4)
  def t2: String @pure = C(1,3)(4).toString
  def t3: Int @pure = C(1,3)(4).hashCode()
  def t4: Boolean @pure = C(1,2)(3) == C(2,3)(1)
}

case class C(x: Int, y: Int)(z: Int)


object CC {
  final class SNode[V](final val v: V)
  
  class INode[V] {
    def f(a: Any): Option[V] = a match {
      case sn: SNode[V] => Some(sn.v) // [*]
    }
  }
  
  // [*] used to trigger a type error. sn.v got a `SingleType(sn, v)` where the `underlying` was `V @pure`
  // Therefore in `Some(sn.v)` the type parameter A of class Some was inferred to `V @pure`. This lead to
  // a type error, Some[V @pure] does not conform to Option[V] (I think that's the issue). In any case,
  // the fix is to have a singleton type for `sn.v` where the the underlying is defined to be V, see
  // `TypeUtils.scala`.
}
