package scala.tools.nsc.effects
package purity

trait ConvertAnnots { this: PurityDomain =>
  import global._
  import lattice._

  lazy val modClass    = rootMirror.getClassByName(newTypeName("scala.annotation.effects.mod"))
  lazy val assignClass = rootMirror.getClassByName(newTypeName("scala.annotation.effects.assign"))
  lazy val locClass    = rootMirror.getClassByName(newTypeName("scala.annotation.effects.loc"))

  lazy val annotationClasses = List(modClass, assignClass, locClass)

  lazy val localClass   = rootMirror.getClassByName(newTypeName("scala.annotation.effects.local"))
  lazy val anyLocObject = rootMirror.getModuleByName(newTermName("scala.annotation.effects.any"))




  /* *************************** *
   * CONVERTING FROM ANNOTATIONS *
   * *************************** */


  def parseAnnotationInfos(annots: List[AnnotationInfo], default: => Effect): Effect = {
    val (modAnns, assignAnn, locAnns) = purityAnnotations(annots)
    val modEff = localityFromAnnotations(modAnns)
    val assignEff = assingEffFromAnnotations(assignAnn)
    val resLoc = localityFromAnnotations(locAnns)

    /* When there is no store or no locality annotation, we use these defaults:
     *  - "@mod()" (no modifications), if there's no annotation in the "store" domain
     *  - "@loc(any)" if there's no locality annotation */
    if (modEff.isEmpty && assignEff.isEmpty && resLoc.isEmpty) default
    else (
      modEff.getOrElse(RefSet()),
      assignEff.getOrElse(Assigns()),
      resLoc.getOrElse(AnyLoc))
  }

  private def purityAnnotations(annots: List[AnnotationInfo]) = {
    val nil: List[AnnotationInfo] = Nil
    ((nil, nil, nil) /: annots) {
      case ((modAnn, assignAnn, locAnn), annot) =>
        annot.atp.typeSymbol match {
          case `modClass`    => (annot :: modAnn, assignAnn, locAnn)
          case `assignClass` => (modAnn, annot :: assignAnn, locAnn)
          case `locClass`    => (modAnn, assignAnn, annot :: locAnn)
          case _             => (modAnn, assignAnn, locAnn)
        }
    }
  }


  private def localityFromAnnotations(annots: List[AnnotationInfo]): Option[Locality] = {
    if (annots.isEmpty) None
    else {
      val fresh: Locality = RefSet()
      val res = (fresh /: annots) {
        case (loc, ann) =>
          // the localities in the arguments of this locality
          val annArgLocalities = ann.args.map(localityOf)
          joinAllLocalities(annArgLocalities, init = loc)
      }
      Some(res)
    }
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





  private def assingEffFromAnnotations(annots: List[AnnotationInfo]): Option[AssignEff] = {
    if (annots.isEmpty) None
    else {
      val eff = joinAllAssignEffs(annots map assignEffFromAnnot)
      Some(eff)
    }
  }

  private def assignEffFromAnnot(annot: AnnotationInfo): AssignEff = {
    lazy val msg = s"Invalid assign annotation: $annot. First argument needs to be  a lcoal variable."
    annot.args.map(localityOf) match {
      case Nil =>
        Assigns()

      case AnyLoc :: _ =>
        AssignAny

      case RefSet(refs) :: locs =>
        refs.toList match {
          case List(SymRef(sym)) =>
            Assigns((sym, joinAllLocalities(locs)))
          case _ =>
            abort(msg)
        }

      case _ =>
        abort(msg)
    }

  }


  /* ************************* *
   * CONVERTING TO ANNOTATIONS *
   * ************************* */


  /*
   * TODO: should avoid unnecessary annotations, eg @assign() when there's something else
   */
  def toAnnotation(eff: Effect): List[AnnotationInfo] = {
    val res = modAnnotation(eff._1) :: assignAnnotations(eff._2) ::: locAnnnotations(eff._3)
    if (res.isEmpty)
      List(AnnotationInfo(modClass.tpe, Nil, Nil))
    else
      res
  }



  private def ref2Arg(loc: VarRef): Tree = loc match {
    case SymRef(sym) => gen.mkAttributedIdent(sym)
    case ThisRef(sym) => gen.mkAttributedThis(sym)
  }

  /* @TODO: seems incorrect, it yields a tree which pretty-prints as
   *   effects.this.any
   *   Select(This(effects), "any")
   * where `effects` is symbol of package `effects`.
   */
  private def anyLocArg = gen.mkAttributedRef(anyLocObject)

  private def modAnnotation(modEff: Mod): AnnotationInfo = modEff match {
    case AnyLoc =>
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


  private def assignAnnotations(assignEff: AssignEff): List[AnnotationInfo] = assignEff match {
    case AssignAny =>
      List(AnnotationInfo(assignClass.tpe, List(anyLocArg), Nil))

    case Assigns(as) =>
      as.toList map { case (sym, loc) =>
        val locArgs = loc match {
          case AnyLoc => List(anyLocArg)
          case RefSet(s) => s.toList.map(ref2Arg)
        }
        val args = gen.mkAttributedIdent(sym) :: locArgs
        AnnotationInfo(assignClass.tpe, args, Nil)
      }
  }

}
