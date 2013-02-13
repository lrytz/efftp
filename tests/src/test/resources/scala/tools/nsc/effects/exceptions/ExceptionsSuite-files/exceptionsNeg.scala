import annotation.effects._

class C {
  var cnd: Boolean = false

  def f(x: Int) = {
    throw new E1
  }

  def t1: Int @throws[Nothing] = f(1)
  def t2: Int @throws = f(1)
  def t3: Int @pure = f(1)

  def t4: Int @pure = try {
    // error is reported here: the caught exceptions are statically known, so we
    // know which ones are allowed inside the try
    throw new E2
  } catch {
    case _: E1 => 1
  }

  // error reported on try, not inside the try. there's no expected effect when
  // checking the try body, because we cannot statically know what exceptions are caught.
  def t5: Int @pure = try {
    throw new E2
  } catch {
    case _: E1 if (cnd) => 1
  }

  // no error here, but a warning. so we have to leave it as a neg test
  def t6: Int @pure = try {
    throw new E2
  } catch {
    case _ => 1
  }

  // don't know what guards will evaluate to, so need to be conservative
  def t7: Int @pure = try {
    throw new E2
  } catch {
    case e: E2 if (cnd) => 1
  }
}

class E1 extends Exception
class E2 extends Exception
