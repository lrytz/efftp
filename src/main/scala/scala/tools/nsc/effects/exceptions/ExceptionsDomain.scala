package scala.tools.nsc.effects
package exceptions

import scala.tools.nsc.Global

abstract class ExceptionsDomain extends EffectDomain {
  import global._

  // needs to be a lazy val - otherwise the `global` field of the lattice is set to `null`!
  lazy val lattice = new ExceptionsLattice {
    val global: ExceptionsDomain.this.global.type = ExceptionsDomain.this.global
  }
  import lattice._

  lazy val throwsClass = rootMirror.getClassByName(newTypeName("scala.annotation.effects.throws"))
  lazy val annotationClasses: List[Symbol] = List(throwsClass)

  lazy val barTrait = rootMirror.getClassByName(newTypeName("scala.annotation.effects.$bar"))

  def parseAnnotationInfos(annots: List[AnnotationInfo], default: => Effect): Effect = {
    def exceptionsOf(tp: Type): List[Type] = tp match {
      case TypeRef(pre, `barTrait`, args) =>
        args.flatMap(exceptionsOf)
      case tp =>
        List(tp)
    }

    val throwsAnns = annots.filter(_.atp.typeSymbol == throwsClass)

    if (throwsAnns.isEmpty) {
      default
    } else {
      ((Nil: Effect) /: throwsAnns)((eff, annot) => {
        val TypeRef(_, _, List(arg)) = annot.atp
        val tps = {
          // @throws without type arguments is translated to @throws[E] where E is a typeRef to an abstract type symbol
          if (arg.typeSymbol.isAbstractType) List(nothingType)
          else exceptionsOf(arg)
        }
        join(eff, tps)
      })
    }
  }

  def toAnnotation(eff: Effect): List[AnnotationInfo] = {
    def toType(eff: Effect): Type = eff match {
      case Nil => nothingType
      case x :: Nil => x
      case x :: xs => typeRef(barTrait.tpe.prefix, barTrait, List(x, toType(xs)))
    }
    List(AnnotationInfo(typeRef(throwsClass.tpe.prefix, throwsClass, List(toType(eff))), Nil, Nil))
  }

  /**
   * Returns
   *   - the list of types caught by the pattern of `caseDef`
   *   - a boolean indicating if the pattern has been analyzed precisely or not
   */
  def typesMatchingPattern(pattern: Tree): (List[Type], Boolean) = pattern match {
    /*** catch one specific exception ***/

    case Bind(_, Typed(Ident(nme.WILDCARD), tpt)) =>
      (List(tpt.tpe), true)
    case Typed(Ident(nme.WILDCARD), tpt) =>
      (List(tpt.tpe), true)


    /*** catch any exception (i.e. Throwable) ***/

    case Ident(nme.WILDCARD) | Bind(_, Ident(nme.WILDCARD)) =>
      (List(throwableType), true)


    /*** alternatives ***/

    case Alternative(trees) =>
      ((List[Type](),true) /: trees)((res, tree) => {
        val (mask, isPrecise) = res
        val (treeMask, treeIsPrecise) = typesMatchingPattern(tree)
        (mask u treeMask, isPrecise && treeIsPrecise)
      })

    case _ =>
      (Nil, false)
  }

  /**
   * Returns
   *   - the list of types which we know are caugth by the case statements `cases`
   *   - a boolean indicating wether the case analysis was precise
   *   - the effect of the righthand-side statements of the cases
   */
  def typesMatchingCases(cases: List[CaseDef], ctx: EffectContext): (List[Type], Boolean, Effect) = {
    ((bottom, true, bottom) /: cases)((res, cdef) => {
      val (mask, isPrecise, catchEff) = res
      val resCatchEff = catchEff u super.computeEffect(cdef.body, ctx)
      if (cdef.guard.isEmpty) {
        val (caseMask, caseIsPrecise) = typesMatchingPattern(cdef.pat)
        (mask u caseMask, isPrecise && caseIsPrecise, resCatchEff)
      } else {
        (mask, false, resCatchEff)
      }
    })
  }

  override def computeEffectImpl(tree: Tree, ctx: EffectContext): Effect = tree match {
    case Throw(expr) =>
      val exprEff = super.computeEffect(expr, ctx)
      exprEff u List(expr.tpe)

    case Try(body, catches, finalizer) =>
      val (mask, maskIsPrecise, catchEff) = typesMatchingCases(catches, ctx)

      // set the expected effect: allow masked effects inside the `try`
      val expectedMasked = {
        if (maskIsPrecise) ctx.expected.map(_ u mask)
        else None
      }
      val bodyEff = super.computeEffect(body, ctx.copy(expected = expectedMasked))
      val bodyEffMasked = lattice.mask(bodyEff, mask)

      val finEff = super.computeEffect(finalizer, ctx)

      bodyEffMasked u catchEff u finEff

    case _ =>
      super.computeEffectImpl(tree, ctx)
  }
}

abstract class ExceptionsLattice extends EffectLattice {
  val global: Global
  import global._

  lazy val errorType            = rootMirror.getClassByName(newTypeName("java.lang.Error")).tpe
  lazy val runtimeExceptionType = rootMirror.getClassByName(newTypeName("java.lang.RuntimeException")).tpe
  lazy val throwableType        = definitions.ThrowableClass.tpe
  lazy val nothingType          = definitions.NothingClass.tpe

  type Effect = List[Type]


  lazy val top: Effect    = List(throwableType)
  lazy val bottom: Effect = List(nothingType)

  def lte(a: Effect, b: Effect): Boolean =
    a.forall(lteOne(_, b))

  // test for nothingType, that way List(nothingType) conforms to List()
  def lteOne(aTp: Type, b: Effect): Boolean =
    aTp =:= nothingType || b.exists(bTp => aTp <:< bTp)

  def mask(orig: Effect, mask: Effect): Effect =
    orig.filter(ex =>     // e.g: ex = IOException
      !mask.exists(m =>   //      m  = Exception
        ex <:< m))        //      since ex <:< m, remove ex

  def join(a: Effect, b: Effect): Effect = {
    def elimSub(tps: List[Type]): List[Type] = tps match {
      case Nil => Nil
      case t :: ts =>
        val rest = elimSub(ts.filter(t1 => !(t1 <:< t)))
        if (rest.exists(t1 => t <:< t1)) rest
        else t :: rest
    }
    elimSub(a ++ b)
  }

  /**
   * think of 'allowed' excepsions
   *   - a allows E1 and E2
   *   - b allows E1 and E3
   *   - the meet(a,b) allows everything which is allowed by both, i.e. E1
   */
  def meet(a: Effect, b: Effect): Effect = {
    val keepA = a.filter(lteOne(_, b))
    val keepB = b.filter(lteOne(_, a))
    // remove duplicates. if a and b both contain some exception type E1,
    // that E1 will end up in both keepA and keepB
    val aNotB = keepA.filterNot(aTp => keepB.exists(bTp => aTp =:= bTp))
    aNotB ++ keepB
  }

  /**
   * Removes from `eff` all unchecked exceptions.
   */
//  def onlyChecked(eff: Effect): Effect =
//    e.filterNot(ex => ex <:< errorType || ex <:< runtimeExceptionType)
}
