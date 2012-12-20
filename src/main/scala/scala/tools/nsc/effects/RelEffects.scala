package scala.tools.nsc.effects

trait RelEffects { self: EffectDomain =>
  import global._
  import lattice._

  lazy val relClass = rootMirror.getClassByName(newTypeName("scala.annotation.effects.rel"))
  lazy val percent = {
    val effPackage = rootMirror.getModuleByName(newTermName("scala.annotation.effects"))
    definitions.getMember(effPackage, newTermName("%").encode)
  }

  /**
   * Returns the relative effects of a method
   */
  def relEffects(sym: Symbol): List[RelEffect] =
    if (sym == NoSymbol) {
      List()
    } else if (sym.rawInfo.isComplete) {
      relFromAnnotation(sym.annotations)
    } else {
      val encl = {
        // for method symbols, the enclMethod is itself
        val enclM = sym.enclMethod
        if (enclM == sym) sym.owner.enclMethod
        else enclM
      }
      relEffects(encl)
    }

  /**
   * Extract the relative effect from a list of annotations
   */
  private def relFromAnnotation(annots: List[AnnotationInfo]): List[RelEffect] = {
    val relAnnots = annots.filter(_.atp.typeSymbol == relClass)
    (List[RelEffect]() /: relAnnots)((eff, annot) =>
      joinRel(eff, readRelAnnot(annot))
    )
  }

  /**
   * Convert a @rel annotation (which might have multiple arguments) to a list of RelEffects
   */
  private def readRelAnnot(relAnn: AnnotationInfo): List[RelEffect] = {
    def paramFun(tree: Tree): (Loc, Option[Symbol]) = tree match {
      case TypeApply(fun, targs) =>
        paramFun(fun)
      case Apply(fun, args) =>
        paramFun(fun)
      case f @ Select(p @ Ident(_), _) =>
        (ParamLoc(p.symbol), Some(f.symbol))
      case f @ Select(t @ This(_), _) =>
        (ThisLoc(t.symbol), Some(f.symbol))
      case p @ Ident(_) =>
        (ParamLoc(p.symbol), None)
      case t @ This(_) =>
        (ThisLoc(t.symbol), None)
      case _ =>
        abort("unexpected tree in @rel annotation: "+ tree)
    }
    relAnn.args.map(arg => {
      val (param, fun) = paramFun(arg)
      RelEffect(param, fun)
    })
  }

  def relToAnnotation(eff: List[RelEffect]): List[AnnotationInfo] = {
    if (eff.isEmpty) Nil
    else {
      val args = eff map(e => {
        val par = e.param match {
          case ThisLoc(c) => This(c)
          case ParamLoc(p) => Ident(p)
        }
        e.fun match {
          case None =>
            par
          case Some(f) =>
            f.paramss.foldLeft[Tree](Select(par, f))(
              // (fun, params) => Apply(fun, params map (p => Typed(Ident(percent), TypeTree(p.tpe))))
              (fun, params) => Apply(fun, params map (_ => Ident(percent)))
            )
        }
      })
      // trees need types for pickling. types should be correct, else later
      // phases might crash (refchecks), so running typer.
      val typedArgs = args map (typer.typed(_))
      List(AnnotationInfo(relClass.tpe, typedArgs, List()))
    }
  }

  def joinRel(r1: List[RelEffect], r2: List[RelEffect]): List[RelEffect] = {
    var res = r1
    for (e2 <- r2) {
      // include e2 into res. First find relative effects in res that have the same param
      val (sameParam, others) = res.partition(e1 => e1.param == e2.param)
      if (sameParam.isEmpty) {
        res = e2 :: others
      } else {
        sameParam.find(_.fun.isEmpty) match {
          case Some(e) =>
            // if there's an existing relative effect which covers all methods, take only that.
            e :: others
          case None =>
            // if e2 covers all methods, take only e2, drop the existing ones with the same param
            if (e2.fun.isEmpty) e2 :: others
            // otherwise, if the same relative effect already exists, drop e2
            else if (sameParam.exists(_.fun == e2.fun)) res
            // otherwise, include e2
            else e2 :: res
        }
      }
    }
    res
  }

  def meetRel(r1: List[RelEffect], r2: List[RelEffect]): List[RelEffect] = {
    var res = List[RelEffect]()
    for (e1 <- r1; e2 <- r2) {
      if      (e1 <= e2) res = e1 :: res
      else if (e2 <= e1) res = e2 :: res
    }
    res
  }

  def lteRel(r1: List[RelEffect], r2: List[RelEffect]): Boolean = {
    r1.forall(e1 => {
      r2.exists(e2 => e1 <= e2)
    })
  }


  case class RelEffect(param: Loc, fun: Option[Symbol]) {
    def <= (other: RelEffect) =
      this.param == other.param && (this.fun == other.fun || other.fun.isEmpty)
  }

  trait Loc
  case class ThisLoc(cls: Symbol) extends Loc
  case class ParamLoc(param: Symbol) extends Loc {
    override def hashCode() = param.hashCode()
    override def equals(other: Any) = other match {
      case ParamLoc(otherParam) => sameParam(param, otherParam)
      case _ => false
    }
  }

  /**
   * True if `a` and `b` denote the same parameter.
   *
   * We cannot just compare the symbols for equality. The reason is that there might be
   * multiple symbols for the same parameter, the one in the MethodType can be different
   * than the one assigned to trees. (When cloning a MethodType, new parameter symbols
   * get created). Therefore, we commpare owner (the method) and name.
   */
  def sameParam(a: Symbol, b: Symbol): Boolean =
    a.owner == b.owner && a.name == b.name
}
