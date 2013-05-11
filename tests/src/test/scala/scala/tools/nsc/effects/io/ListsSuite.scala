package scala.tools.nsc.effects.io

import org.scalatest.FunSuite
import scala.tools.nsc.effects.testing.TestMacros._

import scala.annotation.effects._

class ListsSuite extends FunSuite {

  abstract class EList[+T] {
    def foreach(f: T => Unit): Unit @pure(f)

    def reverse: EList[T] @pure = {
      var res: EList[T] = ENil
      foreach(x => res = ECons(x, res))
      res
    }

    def map[U](f: T => U): EList[U] @pure(f) = {
      var res: EList[U] = ENil
      this.foreach(x => res = ECons(f(x), res))
      res.reverse
    }
  }

  object EList {
    def apply[T](xs: T*): EList[T] @pure = ENil
//      ((ENil: EList[T]) /: xs)((l, x) => ECons(x, l))
  }

  case object ENil extends EList[Nothing] {
    def foreach(f: Nothing => Unit): Unit @pure = ()
  }

  case class ECons[+T](x: T, xs: EList[T]) extends EList[T] {
    def foreach(f: T => Unit): Unit @pure(f) = {
      f.apply(x)
      xs.foreach(f)
    }
  }


  val l1 = EList(1,2,3)
  def mapPure: EList[Int] @pure = l1.map(x => x+1)
//  def mapImp: EList[Int] @pure = l1.map(x => {println(); x+1})



}
