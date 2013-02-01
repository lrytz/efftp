package scala.tools.nsc.effects

import collection.mutable

trait TypeCheckerPlugin { self: EffectChecker =>

  import global._
  import analyzer.{AnalyzerPlugin, Typer}
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
      setEffectAnnotation(tp, eff, rel)
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
     *
     * @TODO: for constructor tpts this always returns `true`
     */
    def tptWasInferred(tpt: Tree): Boolean = tpt.isEmpty || (tpt match {
      case tt @ TypeTree() => tt.wasEmpty
      case _ => false
    })

    /**
     * TODO: doc
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

    override def pluginsTypeSig(tpe: Type, typer: Typer, defTree: Tree, pt: Type): Type = {

      def valOrDefDefEffect(untypedRhs: Tree, pt: Type, enclMethod: Symbol): Effect = {
        val typedRhs = analyzer.transformed.getOrElseUpdate(untypedRhs, typer.typed(untypedRhs, pt))
        domain.inferEffect(typedRhs, enclMethod)
      }

      defTree match {
        case DefDef(_, _, _, _, tpt, rhs) =>
          val sym = defTree.symbol

          def inferMethodEff(rhs: Tree = rhs, pt: Type = pt) = valOrDefDefEffect(rhs, pt, sym)

          def isCaseApply          = sym.isSynthetic && sym.isCase && sym.name == nme.apply
          def isCopy               = sym.isSynthetic && sym.name == nme.copy
          def isCopyDefault        = sym.isSynthetic && sym.name.toString.startsWith(nme.copy.toString + nme.DEFAULT_GETTER_STRING)
          def isCaseModuleToString = sym.isSynthetic && sym.name == nme.toString_ &&
            sym.owner.isModuleClass && sym.owner.companionClass.isCaseClass

          if (sym.isPrimaryConstructor) {
            /* For the primary constructor, we have to include effects from the template:
             *  - constructor body
             *    - we have to type check everything (that also type checks early definitions)
             *    - we have to replace the `pendingSuperCall` by a real super call like in typedTemplate
             *  - fields: the effect of their rhs is added to the return type (see `case ValDef` below)
             *  - early initializer fields: discard them (see constructor body)
             *  - statements in the template: type check them and compute their effects
             *  - effects of trait parent constructors
             */

            val (templ, templateTyper) = templates(sym.owner)

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

//            val typedTemplate = templateTyper.typedTemplate(templ, analyzer.newTyper(templateTyper.context.outer).parentTypes(templ))
            val typedParents = analyzer.newTyper(templateTyper.context.outer).parentTypes(templ)

            val constrBody = rhs match {
              case Block(earlyVals :+ global.pendingSuperCall, unit) =>
                val argss = analyzer.superArgs(typedParents.head) getOrElse Nil
                Block(earlyVals :+ PrimarySuperCall(argss), unit)
              case _ => rhs
            }

            val rhsE = inferMethodEff(rhs = constrBody, pt = WildcardType)

            val fields = templ.body collect {
              case vd: ValDef if !(vd.symbol.isParamAccessor || vd.symbol.isEarlyInitialized) => vd
            }
            val includeFieldsEff = (rhsE /: fields)((e, vd) => {
              val fieldEff = fromAnnotation(vd.symbol.info)
              // no need to keep effect annotations in pickled signatures of fields
              vd.symbol.modifyInfo(removeAllEffectAnnotations)
              fieldEff
            })

            val statements = templ.body.filterNot(_.isDef)
            val statsOwner = templ.symbol orElse sym.owner.newLocalDummy(templ.pos)
            // TODO: would need to handle imports within the class, see typedStats
            val typedStats = statements.map(stat => {
              val typer = analyzer.newTyper(templateTyper.context.make(stat, statsOwner))
              typer.typed(stat)
            })

            val includeStatsEff = (includeFieldsEff /: typedStats)((e, stat) =>
              domain.inferEffect(stat, sym) // TODO: enclosing method: the constructor sym?
            )

            val totalEff = (includeStatsEff /: typedParents.tail)((e, parent) =>
              fromAnnotation(parent.tpe.typeSymbol.primaryConstructor.info)
            )

            updateMethodTypeEffect(tpe, totalEff, List())

          } else if (sym.isConstructor) {
            /* Auxiliary constructors are handled just like ordinary methods. Just need to use WildcardType
             * to type check the body, not pt (which is the class type).
             */
            val rhsE = inferMethodEff(pt = WildcardType)
            updateMethodTypeEffect(tpe, rhsE, List()) // TODO: relative effects?

          } else if (isCaseApply || isCopy) {
            // synthetic methods: apply and copy of case class
            val e = inferMethodEff()
            // @TODO: should translate @rel annotations from the constructor to @rel annotations of the apply method, copy method
            updateMethodTypeEffect(tpe, e, Nil)

          } else if (isCopyDefault || isCaseModuleToString) {
            // default getters of copy method, toString of companion object
            updateMethodTypeEffect(tpe, bottom, Nil)

          } else if (tptWasInferred(tpt)) {
            // if the return type was inferred, also infer the effect
            val e = inferMethodEff()
            val rel = domain.relEffects(sym)
            updateMethodTypeEffect(tpe, e, rel)

          } else {
            // for methods with annotated return types, don't change anything
            tpe
          }

        case ValDef(_, _, tpt, rhs) =>
          // @TODO: we get here multiple times for accessors: field, getter, setter, they all call typeSig on the ValDef
          // make sure it always works. maybe optimize this case.
          val sym = defTree.symbol


          if (!sym.isLocal && tptWasInferred(tpt)) {
            // field with inferred type: add the effect of the field initializer to the return type
            // of the field. enables constructor effect inference.
            val primConstr = templates(sym.owner)._1.body.find({
              case d: DefDef => d.symbol.isPrimaryConstructor
            }).get.symbol
            // relative effects of primary constructor while inferrign the effect of the field definition
            val e = valOrDefDefEffect(rhs, pt, primConstr)
            setEffectAnnotation(tpe, e, Nil)

          } else if (sym.isParamAccessor) {
            // class parameters don't have effects. @TODO by-name params
            setEffectAnnotation(tpe, bottom, Nil)

          } else {
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
      updateMethodTypeEffect(tpe, e, Nil)
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
          updateFunctionTypeEffect(tpe, e, effectiveRel, funSym.enclClass, tree.pos)

        case _ =>
          removeAllEffectAnnotations(tpe)

      } else {
        tree match {
          case DefDef(_, _, _, _, tpt @ TypeTree(), rhs) if !tpt.wasEmpty =>
            if (!rhs.isErroneous) {
              val sym = tree.symbol
              val rhsEff = domain.inferEffect(rhs, sym)
              val annotEff = fromAnnotation(sym.tpe)
              if (!(rhsEff <= annotEff))
                issueEffectError(rhs, rhsEff, annotEff)
            }

          case _ =>

        }
        tpe
      }

  }


  def issueEffectError(tree: Tree, found: Effect, expected: Effect) {
    val msg = "effect type mismatch;\n found   : " + found + "\n required: " + expected
    analyzer.ErrorUtils.issueTypeError(analyzer.NormalTypeError(tree, msg))(typer.context)
    setError(tree)
  }
}