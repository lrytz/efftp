package scala.tools.nsc.effects
package io

abstract class IODomain extends EffectDomain {

  import global._

  val lattice: IOLattice = new IOLattice
  import lattice._

  lazy val ioClass = rootMirror.getClassByName(newTypeName("scala.annotation.effects.io"))
  lazy val noIoClass = rootMirror.getClassByName(newTypeName("scala.annotation.effects.noIo"))

  lazy val annotationClasses: List[Symbol] = List(ioClass, noIoClass)

  def fromAnnotation(annots: List[AnnotationInfo], default: => Effect): Effect =
    if      (annots.exists(_.atp.typeSymbol == ioClass)) true
    else if (annots.exists(_.atp.typeSymbol == noIoClass)) false
    else default

  def toAnnotation(elem: Effect): List[AnnotationInfo] =
    if (elem) List(AnnotationInfo(ioClass.tpe, Nil, Nil))
    else      List(AnnotationInfo(noIoClass.tpe, Nil, Nil))

  private lazy val printNames = List("println", "print").map(newTermName(_))
  private lazy val PredefMClass = definitions.PredefModule.moduleClass

  private def isPrint(sym: Symbol) = {
    sym.owner == PredefMClass && printNames.contains(sym.name)
  }

  override def inferEff(tree: Tree, enclFun: Symbol, set: Effect => Unit, continue: => Unit) {
    tree match {
      case Apply(fun, args) if isPrint(fun.symbol) =>
        set(true)

      case _ =>
        super.inferEff(tree, enclFun, set, continue)
    }
  }
}

class IOLattice extends EffectLattice {
  type Effect = Boolean

  def top: Effect = true
  def bottom: Effect = false

  def join(a: Effect, b: Effect): Effect = a || b
  def meet(a: Effect, b: Effect): Effect = a && b

  def lte(a: Effect, b: Effect): Boolean = b || !a
}
