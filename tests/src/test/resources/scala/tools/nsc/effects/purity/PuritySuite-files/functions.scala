import annotation.effects._
import scala.language.reflectiveCalls

class C {
  trait E
  trait D {
    val x: E
    @local val y: E
    @local val z: D
  }
  
  
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
  
  
  // also test effects (assignments, modifications)
}


