import annotation.effects._

class C {
  def f1(a: Any): C @loc(a) = a match {
    case c: C => c
  }
}
