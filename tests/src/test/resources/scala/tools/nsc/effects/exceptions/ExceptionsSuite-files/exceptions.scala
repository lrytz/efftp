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

  def t3: Int @pure = try {
    throw new E1
    throw new E2
  } catch {
    case _: E1 | _: E2 =>
      1
  }

  def t4: Int @pure = try {
    throw new E1
  } catch {
    case _: Exception | _: E2 =>
      1
  }

  def t5: Int @pure = try {
    throw new E1
    throw new E2
  } catch {
    case _: E1 => 1
    case _: E2 => 3
  }
}

class E1 extends Exception
class E2 extends Exception
