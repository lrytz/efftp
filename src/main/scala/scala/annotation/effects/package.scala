package scala.annotation

package object effects {
  /**
   * This value can be used for specifying a type in an annotation, e.g.
   *   @pure(m.foo(% : Int, % : String))
   */
  def % : Nothing = throw new Error("% should never be called")
}
