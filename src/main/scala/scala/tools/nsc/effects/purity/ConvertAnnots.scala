package scala.tools.nsc.effects
package purity

trait ConvertAnnots { this: PurityDomain =>
  import global._
  import lattice._

  lazy val modClass = rootMirror.getClassByName(newTypeName("scala.annotation.effects.mod"))
  lazy val locClass = rootMirror.getClassByName(newTypeName("scala.annotation.effects.loc"))

  lazy val annotationClasses = List(modClass, locClass)

  lazy val localClass   = rootMirror.getClassByName(newTypeName("scala.annotation.effects.local"))
  lazy val anyLocObject = rootMirror.getModuleByName(newTermName("scala.annotation.effects.any"))




  /* *************************** *
   * CONVERTING FROM ANNOTATIONS *
   * *************************** */


  def parseAnnotationInfos(annots: List[AnnotationInfo], default: => Effect): Effect = {
    val (modAnns, locAnns) = purityAnnotations(annots)
    val modEff = localityFromAnnotations(modAnns)
    val resLoc = localityFromAnnotations(locAnns)

    /* When there is no store or no locality annotation, we use these defaults:
     *  - "@mod()" (no modifications), if there's no annotation in the "store" domain
     *  - "@loc(any)" if there's no locality annotation */
    if (modEff.isEmpty && resLoc.isEmpty) default
    else (modEff.getOrElse(RefSet()), resLoc.getOrElse(AnyLoc))
  }

  private def purityAnnotations(annots: List[AnnotationInfo]) = {
    val nil: List[AnnotationInfo] = Nil
    ((nil, nil) /: annots) {
      case ((modAnn, locAnn), annot) =>
        annot.atp.typeSymbol match {
          case `modClass` => (annot :: modAnn, locAnn)
          case `locClass` => (modAnn, annot :: locAnn)
          case _          => (modAnn, locAnn)
        }
    }
  }


  private def localityFromAnnotations(annots: List[AnnotationInfo]): Option[Locality] = {
    val fresh: Locality = RefSet()
    val res = (fresh /: annots) {
      case (loc, ann) =>
        // the localities in the arguments of this locality
        val annArgLocalities = ann.args.map(localityOf)
        unionLocalities(loc, annArgLocalities)
    }

    if (annots.isEmpty) None
    else Some(res)
  }


  private def unionLocalities(init: Locality, locs: List[Locality]): Locality =
    (init /: locs) {
      case (locA, locB) => locA union locB
    }


  private def localityOf(arg: Tree): Locality = {
    val sym = arg.symbol
    lazy val msg = s"Invalid location: $arg. Valid locations are parameters, local values or `this`."
    arg match {
      case This(_)  =>
        RefSet(ThisRef(sym))

      case Ident(_) =>
        if (sym == anyLocObject) AnyLoc
        else {
          assert(sym.isValueParameter || sym.isLocal, msg)
          RefSet(SymRef(sym))
        }

      case Select(_, _) if (sym == anyLocObject) =>
        AnyLoc

      case _ =>
        abort(msg)
    }
  }





  /* ************************* *
   * CONVERTING TO ANNOTATIONS *
   * ************************* */


  def toAnnotation(eff: Effect): List[AnnotationInfo] = {
    val res = modAnnotations(eff._1) :: locAnnnotations(eff._2)
    if (res.isEmpty)
      List(AnnotationInfo(modClass.tpe, Nil, Nil))
    else
      res
  }



  private def ref2Arg(loc: VarRef): Tree = loc match {
    case SymRef(sym) => gen.mkAttributedIdent(sym)
    case ThisRef(sym) => gen.mkAttributedThis(sym)
  }

  private def modAnnotations(modEff: Mod): AnnotationInfo = modEff match {
    case AnyLoc =>
      /* @TODO: seems incorrect, it yields a tree which pretty-prints as
       *   effects.this.any
       *   Select(This(effects), "any")
       * where `effects` is symbol of package `effects`.
       */
      val anyLocArg = gen.mkAttributedRef(anyLocObject)
      AnnotationInfo(modClass.tpe, List(anyLocArg), Nil)

    case RefSet(refs) =>
      AnnotationInfo(modClass.tpe, refs.map(ref2Arg).toList, Nil)
  }

  private def locAnnnotations(loc: Locality): List[AnnotationInfo] = loc match {
    case AnyLoc =>
      Nil // AnyLoc is the default when parsing the annotations

    case RefSet(s) =>
      List(AnnotationInfo(locClass.tpe, s.toList.map(ref2Arg), Nil))
  }

}
