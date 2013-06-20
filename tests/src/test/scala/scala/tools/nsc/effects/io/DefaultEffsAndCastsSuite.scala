package scala.tools.nsc.effects.io

import org.scalatest.FunSuite
import scala.tools.nsc.effects.testing.TestMacros._

import scala.annotation.effects._

class DefaultEffsAndCastsSuite extends FunSuite {
  test("pure and noIo interact") {
    val t1 = new { def t: Int @noIo = 0 }
    val t2 = new { def t: Int @pure = 0 }
    val t3 = new { def t: Int @io = 0 }

    assert( isSubtype[{def t: Int @pure}](t1))
    assert( isSubtype[{def t: Int @pure}](t2))
    assert(!isSubtype[{def t: Int @pure}](t3))

    assert( isSubtype[{def t: Int @noIo}](t1))
    assert( isSubtype[{def t: Int @noIo}](t2))
    assert(!isSubtype[{def t: Int @noIo}](t3))

    assert(isSubtype[{def t: Int @io}](t1))
    assert(isSubtype[{def t: Int @io}](t2))
    assert(isSubtype[{def t: Int @io}](t3))
  }

  test("combining pure and noIo / io") {
    val t1 = new { def t: Int @noIo = 0 }
    val t2 = new { def t: Int @pure = 0 }
    val t3 = new { def t: Int @io = 0 }

    assert( isSubtype[{def t: Int @pure @noIo}](t1))
    assert( isSubtype[{def t: Int @pure @noIo}](t2))
    assert(!isSubtype[{def t: Int @pure @noIo}](t3))

    assert(isSubtype[{def t: Int @pure @io}](t1))
    assert(isSubtype[{def t: Int @pure @io}](t2))
    assert(isSubtype[{def t: Int @pure @io}](t3))
  }

  test("rel means pure by deafault") {
    val t1 = new { def t: Int @noIo = 0 }
    val t2 = new { def t: Int @pure = 0 }
    val t3 = new { def t: Int @io = 0 }

    assert( isSubtype[{def t: Int @pure()}](t1))
    assert( isSubtype[{def t: Int @pure()}](t2))
    assert(!isSubtype[{def t: Int @pure()}](t3))

    assert( isSubtype[{def t: Int @pure() @noIo}](t1))
    assert( isSubtype[{def t: Int @pure() @noIo}](t2))
    assert(!isSubtype[{def t: Int @pure() @noIo}](t3))

    assert(isSubtype[{def t: Int @pure() @io}](t1))
    assert(isSubtype[{def t: Int @pure() @io}](t2))
    assert(isSubtype[{def t: Int @pure() @io}](t3))
  }


  // test effect casts
  def foo(xs: Int*): Int @pure = {
    val x = 1
    (x /: xs)(_ + _): @unchecked @pure
  }
}
