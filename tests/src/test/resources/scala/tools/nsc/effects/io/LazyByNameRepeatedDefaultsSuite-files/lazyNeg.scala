import scala.annotation.effects._

// doesn't give the error message in the other file: there we don't get into type checking the class because
// errors are reported too early in the compiler.
class LazyFalsy {
  lazy val y: Int @pure = { println(); 1 }
}

