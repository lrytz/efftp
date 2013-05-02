import annotation.effects._

class C {

  def f1: Int @pure = 1
  def f2: Int @pure = 1 + 1
  def f3: Int @pure = f2 + 1

  def f4: Int @mod(any) = 1
  def f5: Int @mod(any) @loc(any) = 1
  def f6: Int @loc(any) = 1

  def f7: Int @pure = {
    var x = 1
    x = 2
    x
  }

  def f8: Int @pure = {
    var x = 1
    def inc() = { x = x + 1 }
    inc(); inc();
    x
  }

  def f9: Int @pure = {
    var x = 1
    def inc(): Unit @assign(x, any) = { x = x + 1 }
    inc(); inc();
    x
  }

  def f10: Int @pure = {
    val x = 1
    var y = x
    def g(): Int @assign(y, any) = {
      var z = y
      def h: Int @assign(y, any) = {
        y = 4
        x
      }
      var res = h
      z = z + 1
      res
    }
    val u = g()
    u + 1
  }

  
  def f11: Int @pure = {
    var x = 1
    def g(): Int @assign(x, any) = {
      {x = 2; "hum"} match {
        case s =>
          x = 3
          4
      }
    }
    g()
  }
  
  def f12: Int @pure = {
    var x = 1
    while(x < 10) {
      x = x + 1
    }
    x
  }
}
