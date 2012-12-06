package scala.tools.nsc.effects.testing

import org.scalatest.FunSuite
import TestMacros._

class MacrosSuite extends FunSuite {
  test("Int is not <:< String") {
    assert(!isSubtype[String](1 + 1))
  }

  test("String <:< Object") {
    assert(isSubtype[Object]("1" + 1))
  }

  test("subtyping with a refinement") {
    class C
    val c1 = new C { def foo: String = "hai" }
    val c2 = new C { def foo: Object = "hui" }
    assert(isSubtype[C { def foo: Object }](c1))
    assert(!isSubtype[C { def foo: String }](c2))
    assert(!isSubtype[C { def foo: Int }](c1))
  }
}
