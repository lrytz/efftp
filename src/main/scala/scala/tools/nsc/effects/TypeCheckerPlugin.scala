package scala.tools.nsc.effects

import collection.mutable

trait TypeCheckerPlugin { self: EffectChecker =>

  import global._
  import analyzer.{AnalyzerPlugin, Typer, AbsTypeError, SilentResultValue, SilentTypeError}
  import analyzer.ErrorUtils.{issueNormalTypeError, issueTypeError}
  import domain._
  import domain.lattice._

  global.addAnnotationChecker(annotChecker)
  analyzer.addAnalyzerPlugin(typerPlugin)

  // overrides are checked during refChecks
  def pluginIsActive(): Boolean =
    global.phase.id <= currentRun.refchecksPhase.id

  object annotChecker extends AnnotationChecker {

    override def isActive(): Boolean = pluginIsActive()

    // @TODO: make sure we end up here only when checking refinements, members of types. effects of method return
    // types for instance should not be checked here. how? maybe inspect the stack trace to get some evidence.
    def annotationsConform(tpe1: Type, tpe2: Type): Boolean = {
      val e1 = fromAnnotation(tpe1)
      val e2 = fromAnnotation(tpe2)

      val rel1 = relFromAnnotation(tpe1)
      val rel2 = relFromAnnotation(tpe2)

      effectsConform(e1, rel1, e2, rel2)._1
    }

    private def effectsConform(e1: Effect, rel1: List[RelEffect],
                               e2: Effect, rel2: List[RelEffect],
                               visited: Set[Symbol] = Set()): (Boolean, Set[Symbol]) = {
      var localVisited = visited

      def checkRelEff(fun: Symbol) = {
        localVisited += fun
        val resTp = fun.info.finalResultType
        val eFun = fromAnnotation(resTp)
        val relFun = relFromAnnotation(resTp)
        val (b, v) = effectsConform(eFun, relFun, e2, rel2, localVisited)
        localVisited = v
        b
      }

      val res = e1 <= e2 && rel1.forall(r1 => {
        lteRelOne(r1, rel2) || {
          r1 match {
            case RelEffect(ParamLoc(sym), None) if sym.isByNameParam =>
              // by-name parameter references can have any effect
              top <= e2

            case RelEffect(loc, None) =>
              r1.applyMethods.forall(checkRelEff)

            case RelEffect(loc, Some(fun)) =>
              if (localVisited(fun)) true
              else checkRelEff(fun)
          }
        }
      })
      (res, localVisited)
    }


    /**
     * annotations-lub, annotations-glb, for effects in refinements: e.g. if(..) fun1 else fun2, we need
     * lub of the fun's effects
     *
     * In fact, with way "lub" is written in the Scala compiler currently, it's impossible to trigger these
     * methods. First, remember that effect annotations can only appear on return types of methods. Therefore
     * a lub of two effects can only be triggered when computing the lub of two method types, which is only
     * possible if the method appears in a refined type.
     *
     * The type of a term never has an effect annotation, so for instance in
     *
     *   if (cond) t1 else t2
     *
     * the types of t1 and t2 will not have any effect annotations - but they might be refined types with
     * effect-annotated methods.
     *
     * When computing the lub, the compiler first checks if one type is a supertype of all others (elimSub) and
     * keeps that as the resulting LUB. In this case, effect annotations are preserved.
     *
     * Otherwise, it triggers the actual lub computation. Without going into details of "def lub1", it seems that
     * the scala compiler doesn't compute refinements of individual members at all, as shown by this repl transcript
     *
     *   scala> class A; class B
     *   scala> if (cnd) new { def f: A = new A } else new { def f: B = new B }
     *   res2: Object = $anon$1@9aa3f3
     *
     * The type of res2 is Object, even though it could be { def f: Object }
     *
     * Note that there's a workaround: manually giving the expected type. Then the lub computation is not triggered,
     * instead the typer will just verify that both branches of the "if" conform to the expected type
     *
     *   scala> (if (cnd) new { def f: A = new A } else new { def f: B = new B }) : { def f: Object }
     *   res3: AnyRef{def f: Object} = $anon$2@457235
     */

    override def annotationsLub(tp: Type, ts: List[Type]): Type =
      lubOrGlb(tp, ts, joinAll, joinAllRel)

    override def annotationsGlb(tp: Type, ts: List[Type]): Type =
      lubOrGlb(tp, ts, meetAll, meetAllRel)


    // @TODO: should only do something if there are effect annotations in "ts", the method is also triggered
    // if there are other kinds of annotations
    def lubOrGlb(tp: Type, ts: List[Type],
                 combineEff: List[Effect] => Effect,
                 combineRel: List[List[RelEffect]] => List[RelEffect]): Type = {
      val effs = ts.map(tp => fromAnnotation(tp))
      // @TODO: check if rel effects all refer to symbols of the resulting method type, i.e. if lub/glb unify the
      // symbols for dependent method types
      val rels = ts.map(tp => relFromAnnotation(tp))
      val eff = combineEff(effs)
      val rel = combineRel(rels)
      setEffect(tp, eff, rel)
    }

  }

  object typerPlugin extends AnalyzerPlugin {
    /**
     * De-activate this annotation checker for phases after pickling.
     * UnCurry for instance generates classes and type-checks them, this code can confuse the
     * annotation checker (example: the generated class doesn't exist in the `templates` map)
     *
     * need to run in parser: many type completers run at parser phase (see LazyTypeRef in UnPickler),
     * and type completion can trigger typing of arbitrary trees
     */
    override def isActive(): Boolean = pluginIsActive()

    // todo: weak map, or clear it. OR: use tree attachments, attach (Template, Typer) to the primary constructor DefDef
    val templates: mutable.Map[Symbol, (Template, Typer)] = mutable.Map()

    lazy val ConstrEffTypeDefName = newTypeName("constructorEffect")


    /**
     * We remove all effect annotations from the expected type when type-checking a tree.
     *
     * @TODO: doc why, see session.md
     */
    override def pluginsPt(pt: Type, typer: Typer, tree: Tree, mode: Int): Type = {
      removeAllEffectAnnotations(pt)
    }

    /**
     * For the `tpt` of a DefDef or ValDef, returns `true` if the tpt was empty, i.e. if
     * the return type of the definition was inferred.
     */
    def tptWasInferred(tpt: Tree): Boolean = tpt.isEmpty || (tpt match {
      case tt @ TypeTree() => tt.wasEmpty
      case _ => false
    })


    /**
     * Returns the annotated effect of a constructor, if an effect annotation exists.
     *
     * Primary constructor effects are annotated on the class. Auxiliary constructor effects
     * are annotated on the constructor symbol
     */
    def annotatedConstrEffect(constrSym: Symbol, typeDefAnnots: List[AnnotationInfo]): Option[(Effect, List[RelEffect])] = {
      val annotatedSym = if (constrSym.isPrimaryConstructor) {
        val clazz = constrSym.owner
        if (clazz.isModuleClass) clazz.sourceModule else clazz
      } else constrSym
      val symAnnots = annotatedSym.annotations

      if (existsEffectAnnotation(symAnnots))
        Some((fromAnnotationList(symAnnots), relFromAnnotationList(symAnnots)))
      else if (existsEffectAnnotation(typeDefAnnots))
        Some(fromAnnotationList(typeDefAnnots), relFromAnnotationList(typeDefAnnots))
      else
        None
    }

    /**
     * 
     */
    def constrEffTypeDefAnnots(constrDef: DefDef, tmpl: Option[Template], typer: Typer, alreadyTyped: Boolean): List[AnnotationInfo] = {

      class MapReferences(symMap: Map[Symbol, Symbol]) extends Transformer {
        override def transform(tree: Tree) = tree match {
          case t if symMap.contains(t.symbol) =>
            gen.mkAttributedRef(symMap(t.symbol))
          case t =>
            super.transform(t)
        }
      }

      tmpl match {
        case Some(Template(_, _, body)) =>
          body collect {
            case td @ TypeDef(_, ConstrEffTypeDefName, _, _) => td
          } match {
            case td :: _ =>
              val paramFields = body collect {
                case vd: ValDef if vd.mods.isParamAccessor => vd
              }
              if (!alreadyTyped) {
                typer.namer.enterSyms(td :: paramFields)
              }
              val fieldSyms = paramFields.map(_.symbol)
              val paramSyms = constrDef.vparamss.flatten.map(_.symbol)
              assert(fieldSyms.length == paramSyms.length && (fieldSyms, paramSyms).zipped.forall((fs, ps) => fs.name == ps.name), s"$fieldSyms --- $paramSyms ")

              val annots = td.symbol.initialize.annotations
              // TODO: for val params, do they refer to the getters instead of the fieldSyms?
              val mapper = new MapReferences(fieldSyms.zip(paramSyms).toMap).transform(_)
              annots map {
                case AnnotationInfo(atp, args, assocs) => AnnotationInfo(atp, args map mapper, assocs)
              }

            case _ =>
              Nil
          }

        case None => constrDef.rhs match {
          case DefDef(_, _, _, _, _, Block((td @ TypeDef(_, ConstrEffTypeDefName, _, _)) :: _, _)) =>
            if (!alreadyTyped) {
              typer.namer.enterSym(td)
            }
            td.symbol.initialize.annotations

          case _ =>
            Nil
        }
      }
    }

    /**
     * Returns the effect of a primary constructor, composed of the effects of the `constrBody`,
     * the field initializers and the statements in `tpl`.
     *
     * @param constrBody   The body of the primary constructor, can be typed or not
     * @param defTyper     The typer for the constructor body
     * @param templ        The template, can be typed or not
     * @param templTyper   Typer for the template
     * @param typedParents The typed parent trees of the template
     * @param alreadyTyped A flag indicating if `body` and `templ` are already typed or not
     * @param expected     The expected effect. If defined, computeEffect will report errors on effect mismatches.
     */
    def inferPrimaryConstrEff(constrDef: DefDef, defTyper: Typer, templ: Template, templTyper: Typer,
                              typedParents: List[Tree], alreadyTyped: Boolean, expected: Option[Effect]): (Effect, List[RelEffect]) = {
      val constrSym = constrDef.symbol
      val constrBody = constrDef.rhs
      
      val constrMismatchMsg = "The effect of the primary constructor does not match the annotated effect."
      val superContextMsg     = Some(constrMismatchMsg + "\nThe mismatch is either due to the super constructor call or an early definition.")
      val fieldContextMsg     = Some(constrMismatchMsg + "\nThe mismatch is due to a field initialization expression.")
      val ownerKindString     = if (constrSym.owner.isModuleClass) "object" else constrSym.owner.keyString // keyString is 'class' for module classes
      val statementContextMsg = Some(constrMismatchMsg + s"\nThe mismatch is due to a statement in the ${ownerKindString} body.")
      val parentContextMsg    = Some(constrMismatchMsg + "\nThe mismatch is due to the initializer of a parent trait.")

      val typedConstrBody =
        if (alreadyTyped) constrBody
        else typeCheckRhs(constrBody, defTyper, WildcardType)
      val anfConstrBody = maybeAnf(typedConstrBody, defTyper, WildcardType)

      val relEnv = relEffects(constrSym)

      val rhsEff = domain.computeEffect(anfConstrBody, effectContext(expected, relEnv, defTyper, superContextMsg))

      val fields = templ.body collect {
        // lazy vals don't contribute to the constructor effect
        case vd: ValDef if !(vd.symbol.isParamAccessor || vd.symbol.isEarlyInitialized || vd.symbol.isLazy) => vd
      }
      val fieldEffs = fields.map({ vd =>
        val fieldSym = vd.symbol
        lazy val fieldTyper = analyzer.newTyper(templTyper.context.makeNewScope(vd, fieldSym))
        val typedFieldRhs =
          if (alreadyTyped) vd.rhs
          else {
            typeCheckRhs(vd.rhs, fieldTyper, fieldSym.tpe)
          }
        val anfFieldRhs = maybeAnf(typedFieldRhs, fieldTyper, fieldSym.tpe)
        val eff = domain.computeEffect(anfFieldRhs, effectContext(expected, relEnv, templTyper, fieldContextMsg))
        (fieldSym, eff)
      }).toMap

      val statements = templ.body filter {
        case Import(_, _) => false // @TODO imports handling
        case s => !s.isDef
      }
      val anfStats = {
        if (alreadyTyped) statements
        else {
          val statsOwner = templ.symbol orElse constrSym.owner.newLocalDummy(templ.pos)
          // TODO: would need to handle imports within the class, see typedStats in Typers
          statements.map(stat => {
            val localTyper = analyzer.newTyper(templTyper.context.make(stat, statsOwner))
            val typedStat = localTyper.typed(stat)
            maybeAnf(typedStat, localTyper, WildcardType)
          })
        }
      }
      val statEffs = anfStats map { stat =>
        domain.computeEffect(stat, effectContext(expected, relEnv, templTyper, statementContextMsg))
      }

      val traitInitEffs = typedParents.tail.map({ parent =>
        // fromAnnotation(parent.tpe.typeSymbol.primaryConstructor.info)
        val traitInit = parent.tpe.typeSymbol.primaryConstructor
        val eff =
          if (traitInit == NoSymbol) lattice.bottom
          else {
            // position important for effect mismatch error messages
            val traitInitApply = atPos(parent.pos)(Apply(gen.mkAttributedRef(traitInit), Nil))
            domain.computeEffect(defTyper.typed(traitInitApply), effectContext(expected, relEnv, defTyper, parentContextMsg))
          }
        (traitInit, eff)
      }).toMap

      val resEff = adaptInferredPrimaryConstrEffect(constrDef, rhsEff, fieldEffs, statEffs, traitInitEffs)
      // need to check that the effect conforms because `adaptInferredPrimaryConstrEffect` might return a larger effect
      // than the expected effect used for checking the various statements
      val ctx = effectContext(expected, relEnv, templTyper, Some(constrMismatchMsg))
      checkConform(resEff, templ, ctx)
      (resEff, relEnv)
    }

    /**
     * The comment below applies to scala/scala branch "master", i.e. the upcoming 2.11. This template desugaring
     * scheme used to be also in 2.10.x, but it got reverted in PR #2068 for binary compatibility. The differences
     * are that in the old scheme, i.e. in 2.10
     *
     *   - Template parents do NOT have value arguments, only type arguments
     *   - Type checking the parent types does not produce any attachments
     *   - The tree of the primary constructor has the correct super constructor call from the beginning, there
     *     is no `pendingSuperCall`. The value parameters of the super constructor call are already there.
     *   - The side-effect of assigning a type to the pre-super fields is now done by `parentTypes` in the old
     *     scheme (`typedPrimaryConstrBody` in the new scheme)
     *
     *
     * Primary constructors get a very special treatment in parsing, naming and typing. Here's an example:
     *
     * class C(param: Int) extends { val early = this } with D[T](arg) with T { self: ST =>
     *   val field: Int = 10
     *   statement1()
     *   def this() {
     *     this(1)
     *     statement2()
     *   }
     * }
     *
     * After parsing, the class is represented as follows (see def Template` in scala.tools.nsc.ast.Trees)
     *
     * ClassDef(C) { Template(D[T](arg), T) {
     *   // "self" field of template
     *   private val self: ST = <empty>
     *
     *   // "body" list of template
     *   <presuper> private[this] val early = <empty>            // note: this field has an empty tpt and no rhs!
     *   <paramaccessor> private[this] val param: Int = <empty>
     *   def <init>(param: Int) = {                              // note: no return type. methodSig for the constructor will put C.
     *     <presuper> val early = this
     *     <pendingSuperCall> super.<init>()                     // no super constructor arguments yet, just a dummy!
     *     ()
     *   }
     *   val field: Int = 10
     *   statement1()
     *   def <init>(): C {
     *     this.<init>(1)
     *     statement2()
     *   }
     * }}
     *
     * Namer assigns a lazy type to class C which calls classSig, and subsequently templateSig.
     *
     * TemplateSig will invoke `typer.parentTypes(tmepl)`. Typing the parent types has a few side-effects
     *   - For the parent type containing the super constructor argument, the returned tree is the typed
     *     parent type without the arguments. The latter are available as a SuperArgsAttachment.
     *   - The typed parent type trees are discarded, templateSig only keeps the computed types. The typer
     *     will call `parentTypes(templ)` once again to get the typed trees.
     *   - If the superclass has type parameters, it is necessary to type-check the primary constructor
     *     to infer those type arguments. (For trait parents this is not necessary, there the type arguments
     *     need to be specified in source).
     *   - The method `typer.typedPrimaryConstrBody` has a crucial side-effect: for every <presuper> value
     *     in the primary constructor, after computing its type it assigns that type to the corresponding
     *     <presuper> field in the class. If that was not the case, the type completers for these fields would
     *     fail: there would be no tpt and no rhs.
     *   - If the superclass is not polymorphic, the <presuper> fields will not have a type. In this case,
     *     `parentTypes` invokes `typedPrimaryConstrBody` to make this happen.
     *
     * When the class is type-checked, typedTemplate will modify the trees to the following
     *   - Introduce the typed template parents (and remove the value arguments)
     *   - introduce the real super call into the primary constructor using `Trees.PrimarySuperCall` and the
     *     arguments stored in the (type-checked) parent type's `SuperArgsAttachment`.
     *   - the presuper values (inside the constructor) and the super call need to be type checked in a
     *     constructor context (outside the scope of the template). This is handled by
     *       - typedValDef, which checks if the value symbol is a constructor param or an early def
     *       - typedArg for super call arguments (SCC "self or super constructor call" mode will be enabled)
     *
     * ClassDef(C) { Template(D[T], T) {
     *   [...]

     *   <presuper> private[this] val early: Int = <empty>   // type added by `typedPrimaryConstrBody`
     *   <accessor> def early: Int = early                   // accessors generated by namer for fields
     *   [...]
     *   def <init>(param: Int): C {                 // return type added by methodSig for constructor def
     *     <presuper> val early: Int = this          // type inferred
     *     super.<init>("")                          // real super call
     *     ()
     *   }
     *   [...]
     * }}
     */
    private def primaryConstrEff(ddef: DefDef, defTyper: Typer): (Effect, List[RelEffect]) = {
      val constrSym = ddef.symbol
      val (templ, templateTyper) = templates(constrSym.owner)

      def inferConstrEff: (Effect, List[RelEffect]) = {

        /* For the primary constructor, we have to include effects from the template:
         *  - constructor body
         *    - we have to type check everything (that also type checks early definitions)
         *    - we have to replace the `pendingSuperCall` by a real super call like in typedTemplate
         *  - fields: the effect of their rhs is added to the return type (see `case ValDef` below)
         *  - early initializer fields: discard them (see constructor body)
         *  - statements in the template: type check them and compute their effects
         *  - effects of trait parent constructors
         */

        def enterTplSyms() {
          val self1 = templ.self match {
            case vd @ ValDef(_, _, tpt, EmptyTree) =>
              val tpt1 = treeCopy.TypeTree(tpt).setOriginal(tpt) setType vd.symbol.tpe
              copyValDef(vd)(tpt = tpt1, rhs = EmptyTree) setType NoType
          }
          if (self1.name != nme.WILDCARD)
            templateTyper.context.scope enter self1.symbol
          templateTyper.namer.enterSyms(templ.body)
        }
        enterTplSyms()

        val typedParents = analyzer.newTyper(templateTyper.context.outer).parentTypes(templ)

        /* FOR 2.11, NEED THE FOLLOWING
        val constrBody = ddef.rhs match {
          case Block(earlyVals :+ global.pendingSuperCall, unit) =>
            val argss = analyzer.superArgs(typedParents.head) getOrElse Nil
            // positions not necessarily important, anyway we don't report any effect mismatch errors here
            val pos = wrappingPos(typedParents.head.pos, argss.flatten)
            val superCall = atPos(pos)(PrimarySuperCall(argss))
            Block(earlyVals :+ superCall, unit)
          case rhs => rhs
        }
        */
        inferPrimaryConstrEff(ddef, defTyper, templ, templateTyper,
                              typedParents, alreadyTyped = false, expected = None)
      }

      val typeDefAnnots = constrEffTypeDefAnnots(ddef, Some(templ), templateTyper, alreadyTyped = false)
      annotatedConstrEffect(constrSym, typeDefAnnots).getOrElse {
        inferConstrEff
      }
    }

    /**
     * Return the typed `rhs` of a DefDef or ValDef.
     *
     * `pt` needs to be a by-name parameter so that its computation (which might trigger
     * completion of a symol) is executed within the `silent` context. Example:
     *   class C { val x = new C }
     *
     * Here, computing the type of x will
     *   - compute type of primary constructor of C, so
     *   - compute the effect of constructor C, so
     *   - compute the effect of the field initializers within C, so
     *   - call `typeCheckRhs` for rhs of `x`
     *
     * The expected type for that `typeCheckRhs` invocation is `xSymbol.tpe`, which triggers a cyclic reference
     * error. By having `pt` a by-name parameter, the cyclic reference is caught and we report a useful error
     * message ("constructor needs effect annotation").
     */
    private def typeCheckRhs(rhs: Tree, rhsTyper: Typer, pt: => Type): Tree = {
      analyzer.transformed.getOrElseUpdate(rhs, {
        try {
          rhsTyper.silent(_.typed(rhs, pt)) match {
            case SilentResultValue(t) => t
            case SilentTypeError(e) =>
              val msg = e.errMsg + "\nType error occured while type checking a tree for effect inference."
              issueTypeError(changeErrorMessage(e, msg))(rhsTyper.context)
              rhsTyper.infer.setError(rhs)
          }
        }
        catch {
          case CyclicReference(sym, info) =>
            val msg = s"Cyclic reference involving $sym.\n" +
              s"This error occured while inferring the effect of $sym, the effect needs to be annotated.\n" +
              s"If this error is reported at a field, the primary constructor needs an effect annotation."
            issueNormalTypeError(rhs, msg)(rhsTyper.context)
            rhsTyper.infer.setError(rhs)
        }
      })
    }

    def maybeAnf(tree: Tree, typer: => Typer, pt: => Type) = {
      if (requireANF) AnfTransformer.transformToAnf(tree, typer, pt)
      else tree
    }


    /**
     * This method allows modifying the type which is assigned to a definition's symbol during Namer.
     * The effect of a method is inferred if also its type is inferred.
     */
    override def pluginsTypeSig(tpe: Type, typer: Typer, defTree: Tree, pt: Type): Type = defTree match {
      case ddef @ DefDef(_, _, _, _, tpt, rhs) =>
        val sym = defTree.symbol

        def inferMethodEff(pt: Type = pt): (Effect, List[RelEffect]) = {
          // since the effect of the mehtod is inferred, the relative effect is inherited from the enclosing method.
          // see comment on `def relEffects`
          val relEnv = relEffects(sym.owner.enclMethod)
          val typedRhs = typeCheckRhs(rhs, typer, pt)
          val anfRhs = maybeAnf(typedRhs, typer, pt)

          val eff = domain.computeEffect(anfRhs, effectContext(None, relEnv, typer))
          (adaptInferredMethodEffect(sym, eff), relEnv)
        }

        def isCaseApply          = sym.isSynthetic && sym.isCase && sym.name == nme.apply
        def isCopy               = sym.isSynthetic && sym.name == nme.copy
        def isCopyDefault        = sym.isSynthetic &&
          sym.name.toString.startsWith(nme.copy.toString + nme.DEFAULT_GETTER_STRING)
        def isCaseModuleToString = sym.isSynthetic && sym.name == nme.toString_ &&
          sym.owner.isModuleClass && sym.owner.companionClass.isCaseClass

        if (sym.isPrimaryConstructor) {
          val (e, rel) = primaryConstrEff(ddef, typer)
          setEffect(tpe, e, rel)

        } else if (sym.isConstructor) {
          /* Auxiliary constructors effects are inferred just like ordinary methods. Just need to use WildcardType
           * to type check the body, not pt (which is the class type).
           */
          val typeDefAnnots = constrEffTypeDefAnnots(ddef, None, typer, alreadyTyped = false)
          val (rhsE, relEffs) = annotatedConstrEffect(sym, typeDefAnnots).getOrElse {
            // Hack, but YEAH. This allows effect inference for auxiliary constructors. `tpt.tpe` is the correct
            // return type for the constructor (without effect annotations). Having a non-lazy type prevents a cyclic
            // reference when resolving the correct overload of the self-constructor-invocation. Since auxiliary
            // constructors can only call earlier ones, this is safe. Nobody will observe the temporary type.
            sym.setInfo(tpt.tpe)
            inferMethodEff(pt = WildcardType)
          }
          setEffect(tpe, rhsE, relEffs)

        } else if (isCaseApply || isCopy) {
          // synthetic methods: apply and copy of case class
          val (e, rel) = inferMethodEff()
          setEffect(tpe, e, rel) // TODO: correct relative effects?

        } else if (isCopyDefault || isCaseModuleToString) {
          // default getters of copy method, toString of companion object
          setEffect(tpe, bottom, Nil)

        } else if (tptWasInferred(tpt)) {
          // if the return type was inferred, also infer the effect
          val (e, rel) = inferMethodEff()
          setEffect(tpe, e, rel)

        } else {
          // for methods with annotated return types, don't change anything
          // @TODO: in case there is no effect annotation on tpe, should we add the default effect (top)?
          // things work either way, but it would be better for documentation.
          tpe
        }

      case vdef @ ValDef(_, _, tpt, rhs) if vdef.symbol.isLazy && tptWasInferred(tpt) =>
        val typedRhs = typeCheckRhs(rhs, typer, pt)
        val anfRhs = maybeAnf(typedRhs, typer, pt)

        // NOTE: if this lazy val is a field the relative environment is NOT the one of the class constructor,
        // but really the one of the enclosing method. this is correct: the lazy val is not evaluated during
        // the constructor, but whenever the field is accessed.
        val relEnv = relEffects(vdef.symbol.enclMethod)
        val e = domain.computeEffect(anfRhs, effectContext(None, relEnv, typer))
        setEffect(tpe, e, relEnv)

      case impl: Template =>
        // typer.context.owner is the class symbol for ClassDefs, the moduleClassSymbol for ModuleDefs
        templates += typer.context.owner -> (impl, typer)
        tpe

      case _ =>
        tpe
    }

    /**
     * Set the effect of field accessors
     */
    override def pluginsTypeSigAccessor(tpe: Type, typer: Typer, tree: ValDef, sym: Symbol): Type = {
      val e = accessorEffect(sym, tpe, tree)
      // For the setter type, remove the effect annotation from the argument type. The reason is that the
      // ValDef's type has an effect annotation (to make constructor effect inference work), which ends
      // up in the parameter type here.
      if (sym.isSetter) {
        val MethodType(List(arg), _) = tpe
        arg.setInfo(removeAllEffectAnnotations(arg.tpe))
      }
      // for lazy vals (with inferred type), we add the rhs effect to the field type in pluginsTypeSig.
      // so the effect annotations are already there, we can just keep them
      if (sym.isLazy) tpe
      else setEffect(tpe, e, Nil)
    }


    /**
     * This method is invoked for every tree which that is type checked.
     *
     * For term trees, we remove all effect annotations from the inferred type. We don't store effects of
     * subtrees in their types.
     *
     * For Function trees, we compute the effect of the function's body and assign to the function
     * symbol a refined function type which has the inferred effect
     *
     * For all method definitions which have an annotated return type (and therefore an annotated effect)
     * we verify that the effect of the body conforms to the annotated effect. In order to do the same for
     * the primary constructor, we have to compute the effect of multiple parts of the class template.
     */
    override def pluginsTyped(tpe: Type, typer: Typer, tree: Tree, mode: Int, pt: Type): Type =
      if (tree.isTerm) tree match {
        case Function(params, body) =>
          val funSym = tree.symbol
          val enclMeth = funSym.enclMethod
          val enclRel = domain.relEffects(enclMeth)

          lazy val bodyTyper = analyzer.newTyper(typer.context.makeNewScope(tree, funSym))
          lazy val respt: Type = {
            if (definitions.isFunctionType(pt))
              pt.normalize.typeArgs.last
            else
              WildcardType
          }
          val anfBody = maybeAnf(body, bodyTyper, respt)

          val e = domain.computeEffect(anfBody, effectContext(None, enclRel, typer))

          // we also compute the effect of the function body assuming there are no relative effects.
          // if that effect is the same (smaller or equal) as the effect e, it means that the body
          // of the function does not actually have the relative effect of the enclosing method.
          // therefore we don't assign any relative effect to the function in that case.
          //
          // this could be improved by only keeping those rel effects that actually occur in the rhs. would
          // require to inspect the rhs tree (or compute the absolute effect once for each relative effect)
          val effectiveRel =
            if (enclRel.isEmpty) Nil
            else {
              val eNoRel = domain.computeEffect(anfBody, effectContext(None, Nil, typer))
              if (eNoRel <= e) Nil else enclRel
            }

          // @TODO: not sure what the owner of the new refinement symbol should be (funSym.enclClass)?
          // @TODO: should also remove effect annotations from tpe (as in the default case below)? probably yes.
          setFunctionTypeEffect(tpe, e, effectiveRel, funSym.enclClass, tree.pos)

        case _ =>
          removeAllEffectAnnotations(tpe)

      } else {
        tree match {
          case ddef @ DefDef(_, _, _, _, tpt @ TypeTree(), rhs) =>
            val meth = tree.symbol

            val expectedEffect: Option[Effect] = {
              if (meth.isPrimaryConstructor) {
                None // primary constr effes are handled separately, see case ClassDef/ModuleDef below
              } else if (meth.isConstructor) {
                val typeDefAnnots = constrEffTypeDefAnnots(ddef, None, typer, alreadyTyped = true)
                // we use `annotatedConstrEffect` to test if the constructor ahs an annotated effect.
                // if that's the case, we read the expected effect from the constructor's return type.
                annotatedConstrEffect(meth, typeDefAnnots).map(_ => fromAnnotation(meth.tpe))
              } else if (!tpt.wasEmpty)
                Some(fromAnnotation(meth.tpe))
              else
                None
            }

            expectedEffect foreach (annotEff => {
              lazy val rhsTyper = analyzer.newTyper(typer.context.makeNewScope(ddef, meth))
              val anfRhs = maybeAnf(rhs, rhsTyper, pt)
              val expected = Some(adaptExpectedMethodEffect(meth, annotEff))
              domain.computeEffect(anfRhs, effectContext(expected, relEffects(meth), typer))
            })

          case _: ClassDef | _: ModuleDef =>
            val templ = tree match {
              case cd: ClassDef => cd.impl
              case md: ModuleDef => md.impl
            }

            treeInfo.firstConstructor(templ.body) match {
              case constrDef: DefDef =>
                val constrSym = constrDef.symbol
                val (_, templTyper) = templates(constrSym.owner)
                val constrDefTyper = analyzer.newTyper(templTyper.context.makeNewScope(constrDef, constrSym))
                val typeDefAnnots = constrEffTypeDefAnnots(constrDef, Some(templ), templTyper, alreadyTyped = true)
                for (annotEff <- annotatedConstrEffect(constrSym, typeDefAnnots)) {
                  // as expected effect we use the one on the return type of the constructor, not the one on the
                  // constructor symbol or the typeDef
                  val expected = Some(adaptExpectedMethodEffect(constrSym, fromAnnotation(constrSym.tpe)))
                  inferPrimaryConstrEff(constrDef, constrDefTyper, templ, templTyper,
                                        templ.parents, alreadyTyped = true, expected = expected)
                }

              case EmptyTree =>
                // no primary constructor, so nothing to check
            }
          case _ =>
            // nothing to check for trees other than method definitions
        }
        tpe
      }

  }

  /**
   * Returns a new effect context.
   *
   * @param expected   The expected effect. If defined and the inferred effect does not conform, computeEffect issues
   *                   an effect mismatch error.
   * @param relEnv     The relative effects of the enclosing method, used for computing the effect of an expression.
   * @param typer      The typer is only used for error reporting. It is no problem to use a typer which does not
   *                   match the context of the tree for which the effect is being inferred.
   */
  def effectContext(expected: Option[Effect], relEnv: List[RelEffect], typer: Typer, detailsMsg: Option[String] = None) = {
    def effectReporter(typer: Typer) = new EffectReporter {
      protected def issueError(tree: Tree, msg: String) {
        issueNormalTypeError(tree, msg)(typer.context)
      }
      protected def setError(tree: Tree) {
        typer.infer.setError(tree)
      }
    }

    EffectContext(expected, relEnv, effectReporter(typer), detailsMsg, patternMode = false)
  }

  def changeErrorMessage(err: AbsTypeError, msg: String): AbsTypeError = new AbsTypeError {
    def errMsg = msg
    def errPos = err.errPos
    def kind = err.kind
  }
}
