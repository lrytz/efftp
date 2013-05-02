import annotation.effects._
import scala.language.reflectiveCalls

class C {
  trait E
  trait D {
    val x: E
    @local val y: E
    @local val z: D
  }
  
  
  // return localities
  
  def f1(d: D): E @loc(d) = {
    val anon = new {
      def appliquer() = d.y
    }
    anon.appliquer()
  }

  def f2(d: D): E @loc(d) = {
    val fun = () => d.y
    fun()
  }

  
  def f3(d: D): E @loc(d) = {
    val fun: (() => E) { def apply(): E @loc(d) } = () => d.y
    fun()
  }

  
  // assignment effects

  def f4(d: D): E @loc(d) = {
    var da = d.z
    val fun = () => {
      da = da.z
      da = d.z.z
    }
    fun()
    da.y
  }
  
  
}


