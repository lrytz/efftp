package scala.tools.nsc.effects

trait TypeUtils { self: TypeCheckerPlugin =>

  import global._
  import domain._
  import lattice.Effect

  /**
   * A singleton type with a given underlying type. The SingleType in the compiler computes `underlying` as
   * `tpe.pre.memberType(tpe.sym).resultType`. This is problematic if the `sym` has effect annotations, then
   * the underlying type will have effect annotations. However, the types assigned to trees are not allowed
   * to have effect annotations, this can trigger spurious type mismatch errors due to the AnnotationChecker.
   * 
   * Therefore, if the `underlying` of a SingleType has some effect annotations, the method `removeAnnotations`
   * returns a `FixedUnderlyingSingleType` instead.
   */
  final class FixedUnderlyingSingleType(pre: Type, sym: Symbol, override val underlying: Type) extends SingleType(pre, sym)
  
  /**
   * Remove annotations with a type in `cls` from the type `tp`.
   */
  private def removeAnnotations(tp: Type, cls: List[Symbol]): Type = tp match {
    case AnnotatedType(annots, underlying, _) =>
      underlying.withAnnotations(annots.filterNot(ann => cls.contains(ann.atp.typeSymbol)))
    
    case st @ SingleType(pre, sym) =>
      val underlying = st.underlying
      val cleanUnderlying = removeAnnotations(underlying, cls)
      if (cleanUnderlying eq underlying) st
      else new FixedUnderlyingSingleType(pre, sym, cleanUnderlying)

    case ExistentialType(quantified, underlying) =>
      val cleanUnderlying = removeAnnotations(underlying, cls)
      if (cleanUnderlying eq underlying) tp
      else ExistentialType(quantified, cleanUnderlying)

    case tp => tp
  }

  /**
   * Replace all effect annotation in `tp` by the ones obtained from `eff` and `rel`.
   */
  private def setEffectAnnotations(tp: Type, eff: Effect, rel: List[RelEffect]): Type = {
    val noEffs = removeAllEffectAnnotations(tp)
    noEffs.withAnnotations(toAnnotation(eff)).withAnnotations(relToAnnotation(rel))
  }

  /**
   * Apply operation `op` on the result type of (method) type `tp`.
   */
  private def transformResultType(tp: Type, op: Type => Type): Type = tp match {
    case MethodType(args, res) => copyMethodType(tp, args, transformResultType(res, op))
    case PolyType(targs, res) => PolyType(targs, transformResultType(res, op))
    case NullaryMethodType(res) => NullaryMethodType(transformResultType(res, op))
    case tp => op(tp)
  }

  /**
   * Remove all effect annotations from `tp`, or from its result type if it is a method type.
   */
  def removeAllEffectAnnotations(tp: Type) = {
    transformResultType(tp, removeAnnotations(_, allEffectAnnots))
  }

  /**
   * Set the effect of of (method) type `tp` by adding annotations to its result type.
   */
  def setEffect(mt: Type, eff: Effect, rel: List[RelEffect]): Type = {
    transformResultType(mt, setEffectAnnotations(_, eff, rel))
  }

  /**
   * Adds the effect `eff` to the function type `funTp` by creating a new (or modifying
   * an existing) RefinementType.
   *
   * Example:
   *  funTp : () => Int
   *  effect: IO
   *  result: (() => Int) { def apply(): Int @io }
   *
   * @param owner is the owner of the newly created refinement class symbol. (@TODO: not
   *   sure what it should be.. just the next enclosing owner? or the enclosing class?)
   *
   * @param pos the position of the newly created refinement class symbol
   *
   * @TODO: do we need something else / additional for curried functions (Int => Int => Int)?
   */
  def setFunctionTypeEffect(funTp: Type, eff: Effect, rel: List[RelEffect], owner: Symbol, pos: Position): Type = {
    import definitions.FunctionClass

    funTp match {
      case TypeRef(_, sym, refArgs) if sym == FunctionClass(refArgs.length - 1) =>
        val decls = newScope
        val res = refinedType(List(funTp), owner, decls, pos)
        val refinementOwner = res.typeSymbol
        val method = refinementOwner.newMethod(nme.apply, pos)
        decls.enter(method)

        val (argtps, List(restp)) = refArgs.splitAt(refArgs.length - 1)
        val args = method.newSyntheticValueParams(argtps)
        val methodType = MethodType(args, setEffect(restp, eff, rel))
        method.setInfo(methodType)

        res

      case refType @ RefinedType(parents, decls) =>
        val method = decls.lookup(nme.apply)
        assert(method != NoSymbol, "unexpected function refinement, no apply method found: "+ refType)

        // cloning the symbol / scope, so that the returned type is not == to `funTp`
        val cloned = method.cloneSymbol
        cloned.setInfo(setEffect(cloned.tpe, eff, rel))
        val newDecls = newScope
        newDecls.enter(cloned)
        copyRefinedType(refType, parents, newDecls)
        
      case ErrorType =>
        funTp

      case t =>
        abort("Function tree with unexpecte type " + t)
    }
  }
}
