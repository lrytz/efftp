import scala.annotation.effects._

class K2a {
  // reported error says "constructor needs effect annotation"
  val z = new K2a()
}

object t {
  def mK: K @pure = new K()
  def mk2: K1 @pure = new K1(1)

}

class K {
  val a = 3
  // error has additional message "type error occured during effect inference"
  val y: K = ""

  // illegal cyclic reference, reported error says "need to annotate constructor effect"
  val x: K = new K()
}

class K1 {
  def this(x: Int) {
    this()
    println()
  }

}

// unfortunately we don't get the nice error message here saying "it happened while computing
// the constructor effect". the reason is that the cyclic reference is triggered in the type
// completer of z, which has its own compiler context - it doesn't help if the context for
// typing the constructor body is silent...
// we had a similar problem in the scala compiler and hacked it by counting the number of
// errors before and after, and printing some additional info if some error was printed. we
// could do the same here, count the errors before and after computing the primary constructor
// effect.
class K2 {
  val z = new K2()
}
