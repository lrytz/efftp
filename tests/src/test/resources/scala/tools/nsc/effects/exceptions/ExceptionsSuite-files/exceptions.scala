import annotation.effects._

class C {
  def f(x: Int) = {
    throw new E1
  }

  def t1: Int @pure = try {
    f(1)
  } catch {
    case _: E1 => 1
  }

  def t2: Int @pure = try {
    throw new E2
  } catch {
    case e: E2 => 1
  }
}

class E1 extends Exception
class E2 extends Exception
