package scala.tools.nsc.effects

import collection.mutable

trait TypeCheckerPlugin { self: EffectChecker =>

  import global._
  import analyzer.{AnalyzerPlugin, Typer, AbsTypeError, SilentResultValue, SilentTypeError}
  import analyzer.ErrorUtils.{issueNormalTypeError, issueTypeError}
  import typer.infer.setError
  import domain._
  import domain.lattice._

  global.addAnnotationChecker(annotChecker)
  analyzer.addAnalyzerPlugin(typerPlugin)

  def pluginIsActive(): Boolean =
    global.phase.id <= currentRun.picklerPhase.id

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

      val res = e1 <= e2 && rel1.forall(r1 => {
        lteRel(List(r1), rel2) || {
          r1 match {
            case RelEffect(loc, Some(fun)) =>
              if (localVisited(fun)) true
              else {
                val resTp = fun.info.finalResultType
                val eFun = fromAnnotation(resTp)
                val relFun = relFromAnnotation(resTp)
                val (b, v) = effectsConform(eFun, relFun, e2, rel2, localVisited)
                localVisited = v
                b
              }

            case _ =>
              // @TODO: here we'd need to get all methods of the type of `loc` and check if their effects conform
              // @TODO: should issue a implementation restriction warning
              lattice.top <= e2
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

    // todo: weak map, or clear it
    val templates: mutable.Map[Symbol, (Template, Typer)] = mutable.Map()



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

    def annotatedConstrEffect(constrSym: Symbol): Option[Effect] = {
      val annotatedSym = if (constrSym.isPrimaryConstructor) {
        val clazz = constrSym.owner
        if (clazz.isModuleClass) clazz.sourceModule else clazz
      } else constrSym
      if (symHasEffectAnnotations(annotatedSym)) Some(fromSymAnnotation(annotatedSym))
      else None
    }

    /**
     * Returns the effect of a primary constructor, composed of the effects of the `constrBody`,
     * the field initializers and the statements in `tpl`.
     *
     * @param constrBody   The body of the primary constructor, can be typed or not
     * @param defTyper     The typer for the constructor body, only used if it is not yet typed
     * @param templ        The template, can be typed or not
     * @param templTyper   Typer for the template, only used if it is not yet typed
     * @param typedParents The typed parent trees of the template
     * @param alreadyTyped A flag indicating if `body` and `templ` are already typed or not
     */
    def inferPrimaryConstrEff(constrSym: Symbol, constrBody: Tree, defTyper: Typer, templ: Template,
                              templTyper: Typer, typedParents: List[Tree], alreadyTyped: Boolean): Effect = {
      val typedConstrBody =
        if (alreadyTyped) constrBody
        else typeCheckRhs(constrBody, defTyper, WildcardType)

      // @TODO: constrSym as enclosing method?
      val rhsE = domain.inferEffect(typedConstrBody, constrSym)

      val fields = templ.body collect {
        case vd: ValDef if !(vd.symbol.isParamAccessor || vd.symbol.isEarlyInitialized) => vd
      }
      val includeFieldsEff = (rhsE /: fields)((e, vd) => {
        val typedFieldRhs =
          if (alreadyTyped) vd.rhs
          else {
            val fieldTyper = analyzer.newTyper(templTyper.context.makeNewScope(vd, vd.symbol))
            typeCheckRhs(vd.rhs, fieldTyper, vd.symbol.tpe)
          }
        val fieldEff = domain.inferEffect(typedFieldRhs, constrSym)
        e u fieldEff
      })

      val statements = templ.body.filterNot(_.isDef)
      val typedStats =
        if (alreadyTyped) statements
        else {
          val statsOwner = templ.symbol orElse constrSym.owner.newLocalDummy(templ.pos)
          // TODO: would need to handle imports within the class, see typedStats
          statements.map(stat => {
            val localTyper = analyzer.newTyper(templTyper.context.make(stat, statsOwner))
            localTyper.typed(stat)
          })
        }

      val includeStatsEff = (includeFieldsEff /: typedStats)((e, stat) =>
        e u domain.inferEffect(stat, constrSym) // TODO: enclosing method: the constructor sym?
      )

      val totalEff = (includeStatsEff /: typedParents.tail)((e, parent) => {
        // fromAnnotation(parent.tpe.typeSymbol.primaryConstructor.info)
        val traitInit = parent.tpe.typeSymbol.primaryConstructor
        val eff =
          if (traitInit == NoSymbol) lattice.bottom
          else {
            val traitInitApply = Apply(gen.mkAttributedRef(traitInit), Nil)
            domain.inferEffect(defTyper.typed(traitInitApply), constrSym)
          }
        e u eff
      })
      totalEff
    }

    /**
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
     * After parsing, the class is represented as follows (see `def Template` in scala.tools.nsc.ast.Trees)
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
    private def primaryConstrEff(ddef: DefDef, defTyper: Typer): Effect = {
      val constrSym = ddef.symbol

      def inferConstrEff: Effect = {

        /* For the primary constructor, we have to include effects from the template:
         *  - constructor body
         *    - we have to type check everything (that also type checks early definitions)
         *    - we have to replace the `pendingSuperCall` by a real super call like in typedTemplate
         *  - fields: the effect of their rhs is added to the return type (see `case ValDef` below)
         *  - early initializer fields: discard them (see constructor body)
         *  - statements in the template: type check them and compute their effects
         *  - effects of trait parent constructors
         */
        val (templ, templateTyper) = templates(constrSym.owner)

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

        val constrBody = ddef.rhs match {
          case Block(earlyVals :+ global.pendingSuperCall, unit) =>
            val argss = analyzer.superArgs(typedParents.head) getOrElse Nil
            Block(earlyVals :+ PrimarySuperCall(argss), unit)
          case rhs => rhs
        }
        inferPrimaryConstrEff(constrSym, constrBody, defTyper, templ, templateTyper, typedParents, alreadyTyped = false)
      }


      annotatedConstrEffect(constrSym).getOrElse {
        // @TODO: maybe have an @infer annotation an infer the constructor effect only if that one is present?
        inferConstrEff
      }
    }

    /**
     * Return the typed `rhs` of a DefDef or ValDef.
     *
     * `pt` needs to be a by-name parameter so that its computation is executed within the `silent` context.
     * Example:
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
              setError(rhs)
          }
        }
        catch {
          case CyclicReference(sym, info) =>
            val msg = s"Cyclic reference involving $sym.\n" +
              s"This error occured while inferring the effect of $sym, the effect needs to be annotated.\n" +
              s"If this error is reported at a field, the primary constructor needs an effect annotation."
            issueNormalTypeError(rhs, msg)(rhsTyper.context)
            setError(rhs)
        }
      })
    }

    /**
     * TODO: doc
     */
    override def pluginsTypeSig(tpe: Type, typer: Typer, defTree: Tree, pt: Type): Type = {

      defTree match {
        case ddef @ DefDef(_, _, _, _, tpt, rhs) =>
          val sym = defTree.symbol

          def inferMethodEff(rhs: Tree = rhs, pt: Type = pt) = {
            val typedRhs = typeCheckRhs(rhs, typer, pt)
            domain.inferEffect(typedRhs, sym)
          }

          def isCaseApply          = sym.isSynthetic && sym.isCase && sym.name == nme.apply
          def isCopy               = sym.isSynthetic && sym.name == nme.copy
          def isCopyDefault        = sym.isSynthetic &&
                                     sym.name.toString.startsWith(nme.copy.toString + nme.DEFAULT_GETTER_STRING)
          def isCaseModuleToString = sym.isSynthetic && sym.name == nme.toString_ &&
                                     sym.owner.isModuleClass && sym.owner.companionClass.isCaseClass

          if (sym.isPrimaryConstructor) {
            setEffect(tpe, primaryConstrEff(ddef, typer), List())

          } else if (sym.isConstructor) {
            /* Auxiliary constructors effects are inferred just like ordinary methods. Just need to use WildcardType
             * to type check the body, not pt (which is the class type).
             */
            val rhsE = annotatedConstrEffect(sym).getOrElse {
              // Hack, but YEAH. This allows effect inference for auxiliary constructors. `tpt.tpe` is the correct
              // return type for the constructor (without effect annotations). Having a non-lazy type prevents a cyclic
              // reference when resolving the correct overload of the self-constructor-invocation. Since auxiliary
              // constructors can only call earlier ones, this is safe. Nobody will observe the temporary type.
              sym.setInfo(tpt.tpe)
              inferMethodEff(pt = WildcardType)
            }
            setEffect(tpe, rhsE, List()) // TODO: relative effects?

          } else if (isCaseApply || isCopy) {
            // synthetic methods: apply and copy of case class
            val e = inferMethodEff()
            // @TODO: should translate @rel annotations from the constructor to @rel on the apply method, copy methods
            setEffect(tpe, e, Nil)

          } else if (isCopyDefault || isCaseModuleToString) {
            // default getters of copy method, toString of companion object
            setEffect(tpe, bottom, Nil)

          } else if (tptWasInferred(tpt)) {
            // if the return type was inferred, also infer the effect
            val e = inferMethodEff()
            val rel = domain.relEffects(sym)
            setEffect(tpe, e, rel)

          } else {
            // for methods with annotated return types, don't change anything
            // @TODO: in case there is no effect annotation on tpe, should we add the default effect (top)?
            // things work either way, but it would be better for documentation.
            tpe
          }

        case impl: Template =>
          // typer.context.owner is the class symbol for ClassDefs, the moduleClassSymbol for ModuleDefs
          templates += typer.context.owner -> (impl, typer)
          tpe

        case _ =>
          tpe
      }
    }

    override def pluginsTypeSigAccessor(tpe: Type, typer: Typer, tree: ValDef, sym: Symbol): Type = {
      val e = accessorEffect(sym)
      // For the setter type, remove the effect annotation from the argument type. The reason is that the
      // ValDef's type has an effect annotation (to make constructor effect inference work), which ends
      // up in the parameter type here.
      if (sym.isSetter) {
        val MethodType(List(arg), _) = tpe
        arg.setInfo(removeAllEffectAnnotations(arg.tpe))
      }
      setEffect(tpe, e, Nil)
    }

    override def pluginsTyped(tpe: Type, typer: Typer, tree: Tree, mode: Int, pt: Type): Type =
      if (tree.isTerm) tree match {
        case Function(params, body) =>
          val funSym = tree.symbol
          val enclMeth = funSym.enclMethod
          val e = domain.inferEffect(body, enclMeth)
          val enclRel = domain.relEffects(enclMeth)

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
              val eNoRel = domain.inferEffect(body, NoSymbol)
              if (eNoRel <= e) Nil else enclRel
            }

          // @TODO: not sure what the owner of the new refinement symbol should be (funSym.enclClass)?
          // @TODO: should also remove effect annotations from tpe (as in the default case below)? probably yes.
          setFunctionTypeEffect(tpe, e, effectiveRel, funSym.enclClass, tree.pos)

        case _ =>
          removeAllEffectAnnotations(tpe)

      } else {
        tree match {
          case DefDef(_, _, _, _, tpt @ TypeTree(), rhs) if !rhs.isErroneous =>
            val meth = tree.symbol

            val expectedEffect: Option[Effect] = {
              if (meth.isPrimaryConstructor)
                None // primary constr effes are checked after typing the template signature
              if (meth.isConstructor && symHasEffectAnnotations(meth))
                Some(fromSymAnnotation(meth))
              else if (!tpt.wasEmpty)
                Some(fromAnnotation(meth.tpe))
              else
                None
            }

            expectedEffect foreach (annotEff => {
              val rhsEff = domain.inferEffect(rhs, meth)
              reportIfEffectsMismatch(annotEff, rhsEff, rhs, typer.context)
            })

          case _: ClassDef | _: ModuleDef =>
            val templ = tree match {
              case cd: ClassDef => cd.impl
              case md: ModuleDef => md.impl
            }

            treeInfo.firstConstructor(templ.body) match {
              case constrDef: DefDef =>
                val constrSym = constrDef.symbol
                for (annotEff <- annotatedConstrEffect(constrSym)) {
                  // the `typer` we pass as typer for `rhs` / `templ` does not have the correct context, but that
                  // doesn't matter. the trees are already typed, `typer` won't be used.
                  val constrEff = inferPrimaryConstrEff(constrSym, constrDef.rhs, typer, templ, typer, templ.parents, alreadyTyped = true)
                  val msg = "The effect of the primary constructor does not match the annotated effect. Note that field\n"+
                            "initializers and statements in the class body add to the primary constructor effect."
                  reportIfEffectsMismatch(annotEff, constrEff, tree, typer.context, msg)
                }

              case EmptyTree =>
            }
          case _ =>
        }
        tpe
      }

  }

  def reportIfEffectsMismatch(expected: Effect, found: Effect, tree: Tree, context: analyzer.Context, detailsMsg: String = "") {
    if (!(found <= expected)) {
      val msg = "effect type mismatch;\n found   : " + found + "\n required: " + expected + {
        if (detailsMsg.isEmpty) "" else "\n"+ detailsMsg
      }
      issueNormalTypeError(tree, msg)(typer.context)
      setError(tree)
    }
  }

  def changeErrorMessage(err: AbsTypeError, msg: String): AbsTypeError = new AbsTypeError {
    def errMsg = msg
    def errPos = err.errPos
    def kind = err.kind
  }
}