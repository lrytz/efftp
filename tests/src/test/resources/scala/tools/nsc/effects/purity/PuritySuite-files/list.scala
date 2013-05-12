import annotation.effects._

abstract class MList[T]

class MNil[T] extends MList[T]

class MCons[T](initX: T, initNext: MList[T]) extends MList[T] {
  @local var next: MList[T] = initNext
  @local var x: T = initX
}

object t {
  def setHead[T](l: MList[T], newHead: T): Unit @mod(l, newHead) = l match {
    case _: MNil[_] =>
      ()
    case c: MCons[T] =>
      // needs effect cast because variable c has locality c, we don't support pattern bindings yet.
      (c: @unchecked @loc(l)).x = newHead
  }
}
