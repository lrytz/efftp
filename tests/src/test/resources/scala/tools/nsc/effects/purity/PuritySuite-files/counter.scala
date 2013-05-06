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
  def t4: Int @mod(any) = incAndRead(c)
}

