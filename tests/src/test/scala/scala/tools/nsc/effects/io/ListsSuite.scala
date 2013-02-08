package scala.tools.nsc.effects.io

import org.scalatest.FunSuite
import scala.tools.nsc.effects.testing.TestMacros._

import scala.annotation.effects._

class ListsSuite extends FunSuite {

  abstract class EList[+T] {
    def foreach(f: T => Unit): Unit @rel(f.apply(%))
  }

  case object ENil extends EList[Nothing] {
    def foreach(f: Nothing => Unit): Unit @pure = ()
  }

  case class ECons[+T](x: T, xs: EList[T]) extends EList[T] {
    def foreach(f: T => Unit): Unit @rel(f.apply(%)) = {
      f.apply(x)
      xs.foreach(f)
    }
  }
}
