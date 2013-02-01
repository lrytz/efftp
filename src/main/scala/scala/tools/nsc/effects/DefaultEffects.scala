package scala.tools.nsc.effects

trait DefaultEffects { self: EffectDomain =>
  import global._
  import lattice._

  import global.{definitions => d}
  import global.{rootMirror => m}

  // todo: scala.Product, scala.Equals
  lazy val classesWithPureMethods: List[Symbol] = List(
    d.ObjectClass,
    d.StringClass,
    d.AnyRefClass,
    m.requiredClass[scala.Product],
    m.requiredClass[scala.Equals],
    d.SerializableClass
  ) ++
    d.ScalaValueClasses

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
