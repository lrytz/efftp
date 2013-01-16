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
   *
   * @TODO: document difference to the method relFromAnnotation
   * @TODO: probably not correct, assumes that symbols with non-inferred types have a non-lazy type; not necessarily true AFAICS. see session.md
   */
  def relEffects(sym: Symbol): List[RelEffect] =
    if (sym == NoSymbol) {
      List()
    } else if (!sym.isMethod) {
      abort(s"expected method when looking up relative effects, got $sym")
    } else if (sym.rawInfo.isComplete) {
      // @TODO: here we basically assume that IF the type of a method is not inferred, THEN it ALWAYS has a
      // non lazy type. Let's hope this is the case - is there a better way to know from a symbol if its type
      // was inferred?
      relFromAnnotation(sym.tpe)
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
   * The relative effect of the (return type of the potential method type) `tpe`.
   */
  def relFromAnnotation(tpe: Type): List[RelEffect] = {
    val annots: List[AnnotationInfo] = tpe.finalResultType.annotations
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
      // trees need types for pickling. types should be correct, else later phases might crash
      // (refchecks), so running typer. need to travel to typer phase for that, otherwise we can
      // end up with an assertion failure ("silent mode is not available past typer")
      val typedArgs = exitingPhase(currentRun.typerPhase){ args map (typer.typed(_)) }
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
        res = sameParam.find(_.fun.isEmpty) match {
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

  def joinAllRel(rels: List[RelEffect]*) = {
    if (rels.isEmpty) Nil
    else (rels.tail :\ rels.head)(joinRel)
  }

  def meetRel(r1: List[RelEffect], r2: List[RelEffect]): List[RelEffect] = {
    var res = List[RelEffect]()
    for (e1 <- r1; e2 <- r2) {
      if      (e1 <= e2) res = e1 :: res
      else if (e2 <= e1) res = e2 :: res
    }
    res
  }

  def meetAllRel(rels: List[RelEffect]*) = {
    if (rels.isEmpty) Nil
    else (rels.tail :\ rels.head)(meetRel)
  }

  def lteRel(r1: List[RelEffect], r2: List[RelEffect]): Boolean = {
    r1.forall(lteRelOne(_, r2))
  }

  def lteRelOne(r1: RelEffect, r2: List[RelEffect]): Boolean = {
    r2.exists(e2 => r1 <= e2)
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
