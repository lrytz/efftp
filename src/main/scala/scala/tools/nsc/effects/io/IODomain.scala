package scala.tools.nsc.effects
package io

abstract class IODomain extends EffectDomain {

  import global._

  val lattice: IOLattice = new IOLattice
  import lattice._

  lazy val ioClass = rootMirror.getClassByName(newTypeName("scala.annotation.effects.io"))
  lazy val noIoClass = rootMirror.getClassByName(newTypeName("scala.annotation.effects.noIo"))

  lazy val annotationClasses: List[Symbol] = List(ioClass, noIoClass)

  def parseAnnotationInfos(annots: List[AnnotationInfo], default: => Effect): Effect =
    if      (annots.exists(_.atp.typeSymbol == ioClass)) IOEffect(true)
    else if (annots.exists(_.atp.typeSymbol == noIoClass)) IOEffect(false)
    else default

  def toAnnotation(eff: Effect): List[AnnotationInfo] =
    if (eff.e) List(AnnotationInfo(ioClass.tpe, Nil, Nil))
    else       List(AnnotationInfo(noIoClass.tpe, Nil, Nil))

  private lazy val printNames = List("println", "print").map(newTermName(_))
  private lazy val PredefMClass = definitions.PredefModule.moduleClass

  private def isPrint(sym: Symbol) = {
    // observed NoSymbol in some situations (annotation constructor invocations)
    sym != NoSymbol && sym.owner == PredefMClass && printNames.contains(sym.name)
  }

  override def computeEffectImpl(tree: Tree, ctx: EffectContext): Effect = tree match {
    case Apply(fun, args) if isPrint(fun.symbol) =>
      IOEffect(true)

    case _ =>
      super.computeEffectImpl(tree, ctx)
  }
}

class IOLattice extends EffectLattice {
  type Effect = IOEffect
  
  case class IOEffect(e: Boolean) {
    override def toString() = {
      if (e) "@io"
      else "@noIo"
    }
  }

  def top: Effect    = IOEffect(true)
  def bottom: Effect = IOEffect(false)

  def join(a: Effect, b: Effect): Effect = IOEffect(a.e || b.e)
  def meet(a: Effect, b: Effect): Effect = IOEffect(a.e && b.e)

  def lte(a: Effect, b: Effect): Boolean = b.e || !a.e
}
