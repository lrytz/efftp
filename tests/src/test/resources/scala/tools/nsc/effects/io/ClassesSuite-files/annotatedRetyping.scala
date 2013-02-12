class C {
  def nt = 0
  def tr = "1"

  List(1,2).map(x => {
    /**
     * The bug was the following. The below creates a tree
     *
     *   Match(Annotated(Tuple2(x,y), @unchecked), ....)
     *
     * For effect inference, the body of the anonymous function is type checked early.
     * The result, the typed tree, is not stored in the `transformed` map - this one is
     * only used for ValDef / DefDef bodies.
     *
     * So in the typing phase, the body of the anonymous function is type checked again.
     *
     * The problem was that `typedAnnotated` assigned a type to the `Annotated` tree
     * (to use it as the `original` of the resulting TypeTree). In the second type check,
     * the typer would therefore assume that the Annotated tree is already typed and not
     * invoke the typer again.
     *
     * This is wrong: `Annotated` trees need to be transformed to `Typed` trees during
     * typing. Also, the body of the `Annotated` tree, the `Tuple2(x,y)` part, is NOT
     * type checked, it does not have symbols and types. `typedAnnotated` just took the
     * untyped `Annotated` tree, assigned it a type (not to children).
     *
     * Fix was: `typedAnnotated` now creates a new `Annotated` tree to be used as the
     * `original` of the returned `TypeTree`. Also it uses the typed body for that.
     *
     * This when type-checking the anonymous function body for the second time, we
     * find the original `Annotated` tree which does not have any types defined.
     */
    val (x,y) = (nt, tr)
    x+1
  })
}
