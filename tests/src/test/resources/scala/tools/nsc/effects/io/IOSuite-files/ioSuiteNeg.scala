import annotation.effects.{io, noIo}

object t {
  var cnd = false

  val ptOrLub2: { def f: Object @noIo } = if (cnd) new { def f: A @io = null } else new { def f: B @noIo = null }
}

class A
class B
