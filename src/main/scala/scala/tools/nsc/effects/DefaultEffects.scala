package scala.tools.nsc.effects

trait DefaultEffects { self: EffectDomain =>
  import global._
  import lattice._

  // some useful aliases
  lazy val ObjectClass       = definitions.ObjectClass
  lazy val StringClass       = definitions.StringClass
  lazy val ScalaValueClasses = definitions.ScalaValueClasses

  // todo: scala.Product, scala.Equals

  lazy val classesWithPureMethods: List[Symbol] = ObjectClass :: StringClass :: ScalaValueClasses

  def defaultInvocationEffect(fun: Symbol): Option[Effect] = {
//    if (fun == NoSymbol)
//      None
//    else {
    val owner = fun.owner

    if(classesWithPureMethods.contains(owner)) {
      Some(bottom)
    } else {
      None
    }
//    }

  }
}
