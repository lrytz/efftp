package scala.tools.nsc.effects

trait DefaultEffects { self: EffectDomain =>
  import global._
  import lattice._

  import global.{definitions => d}
  import global.{rootMirror => m}

  lazy val classesWithPureMethods: List[Symbol] = List(
    d.ObjectClass,
    d.StringClass,
    d.AnyRefClass,
    m.requiredClass[scala.Product],
    m.requiredClass[scala.Equals],
    d.SerializableClass
  ) ++
    d.ScalaValueClasses ++
    d.TupleClass.toList

  lazy val classesWithPureConstructors: List[Symbol] = List(
  ) ++
    d.AbstractFunctionClass.toList ++
    d.FunctionClass.toList



  lazy val pureMethods: List[Symbol => Boolean] = List(
    s => s.isConstructor && classesWithPureConstructors.contains(s.owner)
  )


  def defaultInvocationEffect(fun: Symbol): Option[Effect] = {
    val owner = fun.owner

    if (pureMethods.exists(p => p(fun))) {
      Some(bottom)
    } else if (classesWithPureMethods.contains(owner)) {
      Some(bottom)
    } else {
      None
    }
  }
}
