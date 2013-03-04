import annotation.effects._

class Counter {
  private var c = 0
  def inc(): Unit @mod(this) = {
    c += 1
  }

  def get = c
}

object t {
  def readOnly(c: Counter) = {
    c.get
  }
  def incAndRead(c: Counter) = {
    c.inc()
    c.get
  }

  val c = new Counter

  def t1: Int @pure = readOnly(new Counter)
  def t2: Int @pure = readOnly(c)

  def t3: Int @pure = incAndRead(new Counter)
  def t4: Int @pure = incAndRead(c)
}

/*

abstract class MList[T]

class MNil[T] extends MList[T]

class MCons[T](initX: T, initNext: MList[T]) extends MList[T] {
  @local var next: MList[T] = initNext
  @local var x: T = initX
}

object t {
  def setHead[T](l: MList[T], newHead: T): Unit @mod(l) = l match {
    case _: MNil[_] => ()
    case c: MCons[_] => c.x = newHead
  }
}

*/