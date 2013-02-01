package scala.tools.nsc.effects

trait TypeUtils { self: EffectChecker =>

  import global._
  import domain._
  import lattice.Effect

  /**
   * Remove annotations with a type in `cls` from the type `tp`.
   */
  def removeAnnotations(tp: Type, cls: List[Symbol]): Type = tp match {
    case AnnotatedType(annots, underlying, _) =>
      underlying.withAnnotations(annots.filterNot(ann => cls.contains(ann.atp.typeSymbol)))
    case tp => tp
  }

  def removeAllEffectAnnotations(tp: Type) = removeAnnotations(tp, allEffectAnnots)

  def setEffectAnnotation(tp: Type, eff: Effect, rel: List[RelEffect]): Type = {
    val noEffs = removeAllEffectAnnotations(tp)
    noEffs.withAnnotations(toAnnotation(eff)).withAnnotations(relToAnnotation(rel))
  }

  /**
   * Adds the lattice effect `eff` to the function type `funTp` by creating a new
   * (or modifying an existing) RefinementType.
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
  def updateFunctionTypeEffect(funTp: Type, eff: Effect, rel: List[RelEffect], owner: Symbol, pos: Position): Type = {
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
        val methodType = MethodType(args, updateMethodTypeEffect(restp, eff, rel))
        method.setInfo(methodType)

        res

      case refType @ RefinedType(parents, decls) =>
        val method = decls.lookup(nme.apply)
        assert(method != NoSymbol, "unexpected function refinement, no apply method found: "+ refType)

        // cloning the symbol / scope, so that the returned type is not == to `funTp`
        val cloned = method.cloneSymbol
        cloned.setInfo(updateMethodTypeEffect(cloned.tpe, eff, rel))
        val newDecls = newScope
        newDecls.enter(cloned)
        copyRefinedType(refType, parents, newDecls)

      case t =>
        abort("Function tree with unexpecte type " + t)
    }
  }


  /**
   * Change the annotations in the result type of (method) type `tp`. First
   * removes all annotations in `annotationClasses`, then attaches `annots`.
   */
  def updateMethodTypeEffect(mt: Type, eff: Effect, rel: List[RelEffect]): Type = {
    transformResultType(mt, setEffectAnnotation(_, eff, rel))
  }

  /**
   * Apply operation `op` on the result type of (method) type `tp`.
   */
  def transformResultType(tp: Type, op: Type => Type): Type = tp match {
    case MethodType(args, res) => copyMethodType(tp, args, transformResultType(res, op))
    case PolyType(targs, res) => PolyType(targs, transformResultType(res, op))
    case NullaryMethodType(res) => NullaryMethodType(transformResultType(res, op))
    case tp => op(tp)
  }

}
