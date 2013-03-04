package scala.tools.nsc.effects
package state

trait ConvertAnnots { this: StateDomain =>
  import global._
  import lattice._

  lazy val modClass   = rootMirror.getClassByName(newTypeName("scala.annotation.effects.mod"))
  lazy val storeClass = rootMirror.getClassByName(newTypeName("scala.annotation.effects.store"))
  lazy val locClass   = rootMirror.getClassByName(newTypeName("scala.annotation.effects.loc"))

  lazy val annotationClasses = List(modClass, storeClass, locClass)

  lazy val localClass = rootMirror.getClassByName(newTypeName("scala.annotation.effects.local"))

  lazy val anyLocObject   = rootMirror.getModuleByName(newTermName("scala.annotation.effects.any"))
  lazy val freshLocObject = rootMirror.getModuleByName(newTermName("scala.annotation.effects.fresh"))


  def parseAnnotationInfos(annots: List[AnnotationInfo], default: => Effect): Effect = {
    val (modAnns, storeAnns, locAnns) = stateEffectAnnotations(annots)
    val store = storeEffect(modAnns, storeAnns)
    val loc = locEffect(locAnns)

    /* When there is no store or no locality annotation, we use these defaults:
     *  - "@mod()" (no modifications), if there's no annotation in the "store" domain
     *  - "@loc(any)" if there's no locality annotation */
    if (store.isEmpty && loc.isEmpty) default
    else (store.getOrElse(StoreLoc()), loc.getOrElse(AnyLoc))
  }

  private def stateEffectAnnotations(annots: List[AnnotationInfo]) = {
    // filter and partition `annots` into mod, store and loc annotations
    val n: List[AnnotationInfo] = Nil
    ((n, n, n) /: annots) {
      case ((modAnn, storeAnn, locAnn), annot) =>
        annot.atp.typeSymbol match {
          case `modClass`          => (annot :: modAnn, storeAnn, locAnn)
          case `storeClass`        => (modAnn, annot :: storeAnn, locAnn)
          case `locClass`          => (modAnn, storeAnn, annot :: locAnn)
          case _                   => (modAnn, storeAnn, locAnn)
        }
    }
  }

  def storeEffect(modAnns: List[AnnotationInfo], storeAnns: List[AnnotationInfo]): Option[Store] = {
    val fromMod = ((StoreLoc(): Store) /: modAnns) {
      case (store, ann) =>
        val locs = ann.args.flatMap(locationOf)
        (store /: locs)((store, loc) => loc match {
          case Right(_) => StoreAny
          case Left(loc) => store.include(loc, Fresh)
        })
    }

    val fromStore = ((StoreLoc(): Store) /: storeAnns) {
      case (store, ann) =>
        ann.args.flatMap(locationOf) match {
          case in :: from =>
            in match {
              case Right(_) => StoreAny
              case Left(inLoc) =>
                val fromLoc = locList2Locality(from)
                store.include(inLoc, fromLoc)
            }
          case _ =>
            store // there were errors reported during `locationOf`
        }
    }

    if (modAnns.isEmpty && storeAnns.isEmpty) None
    else Some(joinStore(fromMod, fromStore))
  }

  def locEffect(locAnns: List[AnnotationInfo]): Option[Locality] = {
    val res = ((LocSet(): Locality) /: locAnns) {
      case (loc, ann) =>
        val locs = ann.args.flatMap(locationOf)
        locList2Locality(locs)
    }

    if (locAnns.isEmpty) None
    else Some(res)
  }


  private def locationOf(arg: Tree): Option[Either[Location, AnyLoc.type]] = {
    val sym = arg.symbol
    lazy val msg = s"Invalid location: $arg. Valid locations are parameters, local values or `this`."
    arg match {
      case This(_)  =>
        Some(Left(ThisLocation(sym)))

      case Ident(_) =>
        if (sym == anyLocObject) Some(Right(AnyLoc))
        else if (sym == freshLocObject) Some(Left(Fresh))
        else {
          assert(sym.isValueParameter || sym.isLocal, msg)
          Some(Left(SymLocation(sym)))
        }

      case Select(_, _) if (sym == anyLocObject) =>
        Some(Right(AnyLoc))
      case Select(_, _) if (sym == freshLocObject) =>
        Some(Left(Fresh))

      case _ =>
        abort(msg)
        None
    }
  }

  private def locList2Locality(l: List[Either[Location, AnyLoc.type]]): Locality = {
    if (l.exists(_.isRight)) AnyLoc
    else if (l.isEmpty) LocSet(Fresh)
    else LocSet(l.map(_.left.get).toSet)
  }





  def toAnnotation(eff: Effect): List[AnnotationInfo] = {
    val res = storeAnnotations(eff._1) ::: locAnnnotations(eff._2)
    if (res.isEmpty)
      List(AnnotationInfo(modClass.tpe, Nil, Nil))
    else
      res
  }


  def location2Arg(loc: Location): Tree = loc match {
    case Fresh => gen.mkAttributedRef(freshLocObject)
    case ThisLocation(sym) => gen.mkAttributedThis(sym)
    case SymLocation(sym) => gen.mkAttributedIdent(sym)
  }


  def storeAnnotations(storeEff: Store) = storeEff match {
    case StoreAny =>
      /* @TODO: seems incorrect, it yields a tree which pretty-prints as
       *   effects.this.any
       *   Select(This(effects), "any")
       * where `effects` is symbol of package `effects`.
       */
      val anyLocArg = gen.mkAttributedRef(anyLocObject)
      List(AnnotationInfo(modClass.tpe, List(anyLocArg), Nil))

    case StoreLoc(effs) =>
      val (modOnly, others) = effs.partition(e => e._2.s.isEmpty || e._2.s.toList == List(Fresh))
      val store = {
        (for ((loc, set) <- others) yield {
          val in = location2Arg(loc)
          val from = set.s.toList.map(location2Arg)
          AnnotationInfo(storeClass.tpe, in :: from, Nil)
        }).toList
      }

      if (modOnly.isEmpty) store
      else AnnotationInfo(modClass.tpe, modOnly.map(e => location2Arg(e._1)).toList, Nil) :: store
  }

  def locAnnnotations(loc: Locality) = loc match {
    case AnyLoc =>
      Nil // AnyLoc is the default when parsing the annotations

    case LocSet(s) =>
      List(AnnotationInfo(locClass.tpe, s.toList.map(location2Arg), Nil))
  }
}
