import annotation.effects._
import scala.language.reflectiveCalls

class C {
  trait E
  trait D {
    val x: E
    @local val y: E
    @local val z: D
  }
  
  
  def f1a(d: D): E @loc() = {
    val anon = new {
      def appliquer() = d.y
    }
    anon.appliquer()
  }
  def f1b(d: D): E @loc() = {
    val anon = new {
      def appliquer(): E @loc() = d.y
    }
    anon.appliquer()
  }
 
  def f2(d: D): E @loc() = {
    val fun = () => d.y
    fun()
  }

  def f3(d: D): E @loc() = {
    val fun: (() => E) { def apply(): E @loc() } = () => d.y
    fun()
  }
  

  def f4(d: D): E @loc() = {
    var da = d.z
    val fun: (() => E) { def apply(): E @loc() } = () => {
      da = da.z
      da = d.z.z
    }
    fun()
    da.y
  }

}

