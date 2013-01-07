# next up


- Testing
  - test that something compiles
  - test that something doesn't compile with a certain error message (or, with a certain expected / found type)
  - check inferred effect: give a tree and the expected effect


- Function trees, give refined types

- Function: no way to specify rel effect. can specify rel effect in expected type's apply method, but that's not
  an outer method of the function's body.

- getter / setter effect

- EffectChecker that verifies effects of annotated methods, generates effect typing errors
  => only non-inferred types...
  => do we really need a separate traversal? why not just using addAnnotations on DefDef trees?


- keep effect annotations around in tree types, where every effect annotation should describe the effect of that sub-tree?
  + effects are already there, just need to check them
  + in line with how type checking works
  - lots of annotations in trees
  - effects don't propagate like types. example: for a If(cond, tp, ep), the type checker would compute the lub
    of the types of "tp" and "ep" => that type has an effect annotation, but it's worng. so we need to go ahead and
    fix it (in addAnnotations).


- expected type has annotation (DefDef, ValDef, ...)
  > cannot use "adaptAnnotations": this is triggered when the inferred type has annotations

  > we cannot use the expected type's effect annotations to check for the latent effect of a tree: only DefDefs have
    latent effects. However, when there is (for some reason) an effect annotation on a ValDef tpt, or a parameter type,
    then we should not issue effect errors on the corresponding tree (ValDef rhs, method argument).

  > (we should issue warnings when there are effect annotations in places where they don't make sense, see previous point)

  > in typed(tree, mode, pt) we don't know "where we are", if it's a rhs of a DefDef, or a ValDef, or something else.
    So we can't distinguish if effect mismatches should be reported or not

  > therefore we ignore effect annotations on the pt. We actually have to eliminate them because the type computed for
    the tree will not have any annotations, and then <:< in adapt will trigger "annotationsConform" (if pt is annotated),
    which will produce a type error.



- check relative effects in subtyping
  -> still need "inferOnly"? because "annotationsConform" is called whenever there's some annotation present.
     - when there's a return type of a defdef that has an annotation
     - an annotated type (x: Foo @eff)

- refchecks should verify that all overrides are valid - write tests
- computeEffect: when hitting a def tree (but not a valdef!), skip (definitions don't have effects), EffectChecker.scala, line 857






# current architecture

- the current design for re-typing is flawed because it doesn't use an expected type. therefore it uses
  expected type "wildcard". problematic for "ptOrLub". the "ptOrLub" in typers returns "lub" if the pt is
  (or contains) wildcard. so we have a problem with if expressions where the lub is not precise enough, and
  then the computed type doesn't match the annotated one.



# better when doing it during typing

- seems good because the typer propagates some effect annotations wrongly without an annotationChecker
  enabled (lub example, the smaller type is picked, but effects are not considered without annotChecker).

- also seems good because we avoid duplicating work: no re-typing due to effects / effectful types, no
  second subtype-checking for ValDef / DefDef's, ...



# problems with doing it during typing

- problem: for methods / values where the type is *not* inferred (because it's annotated), the
  typer will use that annotated type for all references to the symbol.

  > getter / setter have no annotated effect, but we have to assume some effect. can probably be solved
    by indirection: when taking the effect of a method, check if it's a getter / setter, and return the
    apropriate effect annotation. extend annotChecker with getterEffect to make it configurable (getters
    and setters DO need effect annots for state effects)

  > getter / setter might even get a refined type? then we are in trouble.

  > we can't infer an effect (or a refined type) when type is annotated (even when nested/private!) -
    all references to the symbol would be typed with the original type. probably OK.

  > we need to hack the namer to make `@infer` or `@refine` work - otherwise that symbols will not
    have lazy types. we cannot recognize those annotations symbolically.

  > more serious problem: nested methods implicitly have @rel effects of the enclosing, but that's
    not visible in their type. so references to the symbol won't give a type with the @rel
    > possible to do it manually by looking at owner chain, and all @rel in there? note that we cannot
      force all the outer's types, that would lead to cyclic references. BUT: anyway, finding @rel
      should not force types (no type == no rel)

- we can't infer the effect (or refined type) of recursive definitions: the typer architecture
  does not support it (cyclic reference errors)

- we have to compute effects during typing. the effet of a method body depends on the @rel
  annots of that method. in order not to have cyclic references, we can't force the type of
  the method (if it's inferred) to check if some invocation is a param-call

- effect annotations need to be type constraints to survive asSeenFrom, when the effect annotation appears
  inside a type, in a refinement: the effect should be kept.
  however, that mechanism is wrong for effect annotations that appear on a return type, not inside a
  refinement:

    class C { def f(): Int @noEff = 1 }
    def foo = getC().f()
  
  the effect of method `foo` depends on the effect of `getC()` - however, ordinary type checking would
  propagate the annotation `@noEff` to the type of `foo`. The propagation of effects is checked later
  using the EffectChecker phases. Therefore, effect annotations should only propagate inside types,
  but not on top-level. This is achieved (ab-) using the method `addAnnotations` in the AnnotationCheckers,
  which is called on every type-checked tree. It will remove top-level effect annotations from a tree's
  type.




# design points

every domain needs to define

 - a lattice (join, meet, bot, top, subEff)
 - a compute-effect traverser
 - annotation class (classes?), from- and toAnnotation

effect domains *can* define

 - effect of getter / setter
 - effect of non-annotated methods
 - predicates inferEffect / inferConstructorEffect
 - predicate inferRefinedType

nested methods
  - nested methods inherit relative effects

register effect domains dynamically - similar to annotationCheckers

  - question: use the super-domain which checks single domains element-wise?
    or make this part of the implementation, without explicitly defining the
    super-domain?

implement everything for relative effects directly, don't use the effect domain
abstractions. relative effects are always enabled.

  - relative effects are just annotations (typeConstraints), but not an effect
    system, no conformance checking. (see also TODO section below!!!)
  - no more `anyPC`
  - BUT: need lattice for relative effects: relative effects are subject to LUB/GLB.
    Example: `if (..) v1 else v2` where
      v1: {def f: @rel(a)}
      v2: {def f: @rel(b)}
    then we need to compute the LUB of the two @rel; also done in the paper!
  - implement annotationsLub / annotationsGlb for all effect domains, also for
    relative effects
  - allow @rel(f) annotation when f is a function type, alias for @rel(f.apply(%))
  - allow effect annotation on function type (A => B @eff), also Function1[A, B @eff],
    since it's parsed the same. GOOD: (A => B @eff) is parsed as (A => (B @eff))


have one single AnnotationChecker that implements effect checking for all domains.
register it only during the effect checking phase - not during normal typing.

  - think about it - will that work? can we do an unmodified type-checking first
    and then effect checking after, or is there information that can't be recovered?
    ==> see discussion below (TODO section, later)

subtyping: take relative effects into account. a relative effect can be covered
by a concrete one. this needs to be checked in every domain (or in the element-wise
super domain?)

  - think about it: does this play well with extensible effect domains? can we
    run into problems if effect domains are once enabled, once not?

syntax for effect casting




# things to investigate

  - where do lubs / glbs come into play? probably only in refinements, e.g. when having
    two refined types
  - somewhere: check @rel annotations, that they only refer to symbols in scope



# Todo's, not forget

  - when refining a type with an effect annotation on some method, make sure that the refined method
    does not have a more specific return type! E.g.

      val f: String => Object = (x: String) => x

    the refined type should be as follows, i.e. return type Object!!!

      (String => Object) { def apply(x: String): Object @pure }

  - check: where can refinements occur? adding refinements should always work, i.e. also when
    the inferred / annotated type is already a refinement. can refinements be only at the end
    of a type, or also inside? (A with B) { refine }    <<===>>    (A { refine }) with B

  - when applying a method, the @rel annotations in the return type which refer to parameters
    need to be replaced by concrete effects.
      ==> done in traversal that computes the effectful types
      ==> done in every effect system
      ==> also done for @rel effects!!!
        ==> is this correct? can we do it for @rel effects separately, independent of concrete effects? it seems so in the PDF/paper.

  - "f-like-f" relative effects
      - they are not annotated. but when looking up the relative effect of a method q.m, then @rel(q.m) can be returned.
      - depends on the environment, see pdf report

  - application effect can only be ignored when current method has @rel effect, not an outer method!
    (is already like that in current code)

  - subtyping (for concrete effect systems): need map between parameter names (see paper)? or already done by subtyping algorithm, does
    it replace symbols in the return type, and in annotations? then which one is used, left or right? that matters!!!

  - LUB / GLB: same problem as above! paper uses a map from @rel arguments to the arguments of the new type!!!

  - untyping: can we use something that already exists? should we ever delete TypeApply? can we identify these cases?

  - @rel implementation
    - uses an AnnotationChecker (lub/glb, also to infer annotations in nested methods)
      ===> no need for checker transformation / no error messages
      ===> but need AnnotationChecker for LUB, GLB in return types in refinements
        ==> also use the AnnotChecker ("addAnnotations", case DefDef) to add @rel annotations to nested methods
    - infer @rel: nested methods, function literals: when there's no @rel and no effect annotation.
    - QQQ: when infer @rel in anonymous functions, where to put the annotation? => on the type <=

  - every effect domain has AnnotationChecker (lub/glb) (or one single one for all domains???)
    - don't issue error messages; that's done in the checker traversal!
    - need to be TypeConstraints? (yes: see below)
    - annotationsConform depends on @rel annotations - how to make sure these are already computed / available?
      - probably ok for symbols where the return type is inferred: then "typed" is used, which
        calls into "addAnnotations"
      - however, might be problematic when type is NOT inferred. then the type is computed through
        "typedType", and there we only get access to the type tree, but not to the symbol --> cannot
        check where we are, if nested in anoter eff-poly method
      ==> SOLUTIONS: don't do effect checking during type checking, but using later phases, see discussion below





# old stuff

  - annotation for relative effects: @rel (alternatives: @like, @l, @r, @as, @pc)
  - flow sensitivity: throw out (for now, can keep current state in a branch)




## Type Constraints discussion

1. Effects (also relative effects) need to be TypeConstraints. Otherwise, the effect annotations
get erased in AsSeenFrom, i.e. when a value is selected, the type of the selection does not have
annotations. But we want to keep types *inside* the type, example

  val f1: (A => B) { def apply(a: A): B @effekt } = .....
  val f2 = this.f1

The type of expression `this.f1` should still have the `@effekt` annotation. Therefore they need
to be TypeConstraints

2. However, top-level effect annotations on a type should *not* be propagated. This is a special
case for effects, because they behave differently than other types. For instance, in

  def f(): A @effekt = ...
  val x = f()

the type of `x` should simply be `A`, not `A @effekt`. Similarly, in

  class C { def f(): Int @noEff = 1 }
  def foo = getC().f()

the effect of method `foo` depends on the effect of `getC()` - however, ordinary type checking would
propagate the annotation `@noEff` to the type of `foo`. The propagation of effects is checked later
using the EffectChecker phases. Therefore, effect annotations should only propagate inside types,
but not on top-level. This is achieved (ab-) using the method `addAnnotations` in the AnnotationCheckers,
which is called on every type-checked tree. It will remove top-level effect annotations from a tree's
type.







## Why not do everything during normal type-checking

0. to decide wether or not to infer an effect / refined type, we look at annotations, and that's
   not possible in the namer phase -- the annotations are not yet typed / availalbe. it would be
   very tedious.

1. When to infer effects and refined types: ordinary types are inferred when there's no type annotation.
However, we would like to infer effects and refined types in other situations
 - local definitions, when there's no explicit effect annotation
 - when there's an @infer / @refine annotation

  2. Computing / inferring effects depends on relative effect annotations. The AnnotationChecker for relative
  effects can add @rel annotations when types are inferred. Then the AnnotationChecker for effects should
  already see the @rel annotations - can this be ensured? Would need an order how to apply AnnotationCheckers.

3. Type inference does not work when there are cyclic dependencies. We might however want to infer effects
in methods that are in a cyclic dependency in the future.

4. As a general principle, it might be beneficial to keep things separate.










## traversals

1. for all defdef, assign lazy type if effect needs to be inferred
    -> computeEffect
    -> not for @rel effects!!!

2. for all defdef/valdef, assign lazy type if refinement needs to be inferred
    -> computeType
    -> careful: never do it when type was given explicitly, otherwise an explicit type is replaced with an inferred one!
       OR: should we still do it in some cases? then we should ONLY add refinements, and in those ONLY refine effects (not any types)
    -> also for @rel effects!!!

2a. Do we need to do something similar to 1. and 2. for Function trees? Probably not, because nobody can reference
    a Function, except if it's assigned to a valdef / returned in a defdef.

3. traverse entire trees. for every funcion definition / value definition (statement in a block???), start at the root,
   if there's a reference to a symbol with changing type, remove types on the path and re-typecheck
    -> trees get new types
    -> for method invocations: replace @rel annotations to params in the return type by concrete argument effects
    -> for selections on parameters ("f.apply"), assign a new type with @rel(f) instead of the actual effect of "f.apply", i.e.
       do the f-like-f stuff. don't do it always, depends on environment, see paper.
         def m(f: A => B): B @rel(f) = {
           f.apply(someA)   // f.apply has method type (x$1: A): B @rel(f)
         }
    -> verify: is type-checking the path always enough? or do we need to re-typecheck the entire definition / statement?
    -> call "typed" with an expected type! every call to effTyped needs an expected type
      * for DefDef, ValDef, it's the annotated type
      * for a Function's righthand side, it's the current return type of the function.
    -> also for @rel effects!!!

   the trees with better types are required for computeEffect (and obviously for computeType)

4. special treatment for Function trees.
    -> the return type of the function changes if the type of the righthand-side changes
      -> also for @rel effects!!!
    -> the effect of the righthand side changes the type of the function: we need to add a refinement.
      -> only for concrete effects

5. have annotationChecker in place to
    -> infer GLB/LUB
    -> delete plain effect annots from tree's types (adaptAnnotations? addAnnotations? other way?)
    -> don't issue error messages (inferonly)
    -> what else does the annotationChecker do / achieve?

6. have a checker traversal that verifies effects
    -> verify annotated and computed effect
    -> compare suptyping for definitions with annotated type, do the effects in the refinements conform? => uses annotation checkers!?


7. A nested defdef inherits the @rel annotations from the outer method, unless it has a @rel annotation itself.
    -> TODO: When should this be done exactly? during the first traversal, copy all @rel to inner methods?
    -> Or using the AnnotationChecker, "addAnnotations", "case DefDef"?

  ==> valdefs are NOT a barrier to this, e.g. in
      
    def m(f: A => Unit): Unit @rel(f) = {
      val v = {
        def i() {      // method i() is implicitly @rel(f)
          f(someA)
        }
        i()
        0
      }
      v
    }

  ==> classdefs are also not a barrier to this, e.g. in

      def m(f: A => Unit): Unit @rel(f) = {
        val innerObj = new C {       // value get type C { def method(): Unit @rel(f) }
          def method(): Unit = {     // method implicitly has @rel(f)
            f(this.getA)
          }
        }
      }

  ==> anonymous Functions are also not a barrier to this

      def m(f: A => Unit): Unit @rel(f) = {
        val g = () => {
          def foo() = {   // method foo() implicitly has @rel(f)
            f(someA)
          }
        }
      }

8. Also nested Function inherit the @rel annotations from the enclosing defdef. For example, in
      this.foreach(x => builder.add(f.apply(x)))
   we want the function to have @rel(f) effect

    def m(f: A => Unit): Unit @rel(f) = {
      val fun = () => f(someA)              // fun should have type F0[Unit] { def apply(): Unit @rel(f) }
      fun()
    }

  ==> Similar to above, valdefs / defdefs / other functions are not barriers to this

9. Only nested method definitions inherit the @rel annotations, but not method declarations in refinements inside a method,
   see 10. below

10. Giving a refinemnet type with a different @rel annotation

  def m(f: A => Unit @eff): Unit @eff = {
    val c: C { def foo(): Unit @eff } = {
      new C {
        def foo() = f(someA)
      }
    }
    c.foo()
  }
  
  this is ok, in the sense "as expected, makes sense"
    - "def foo()" gets implicitly the annotation @rel(f), because the outer method has it
    - the "new C {...}" epxression gets a refined type with "rel(f)" in the refinement for "foo()"
    - there is no subtyping check for @rel annotations, so the inferred refinement (with @rel(f)) is allowed
      even if the annotated refinement has @rel()
    - the call "c.foo()" will have effect @eff, because "c.foo" resolves to the symbol in the refinement
  
    - the concrete effect of "f(someA)" is ignored, i.e. foo() only has the relative effect, no concrete effects
    - therefore, checking subtyping will test
        C { def foo(): Unit @rel(f) }   <:<   C { def foo(): Unit @eff }
      which succeeds - the relative effect is covered by the concrete effect
    - so everything is cool.

11. similar situation with functions
  
  def m(f: A => Unit @eff): Unit @eff = {
    val fun: (() => Unit) { def apply(): Unit @eff } = {
      () => f(someA)
    }
  }
  
  what happens
    - the inferred function type is (() => Unit) { def apply(): Unit @rel(f) }
    - checking subtyping with the annotated type:
        (() => Unit) { def apply(): Unit @rel(f) }   <:<   (() => Unit) { def apply(): Unit @eff }
      is good
    - in the same way, we're cool.







## Remarks

1. can @rel annots escape the scope of the parameters? No. @rel annotations are replaced by concrete
effects / other @rel annotations on function invocation. In the following, when applying "g", the
@rel(f) annotation is replaced by the effect of the function passed to "g".

  def f() = {
    def g(f: A => B): A => B @rel(f)
  }



2. Main building blocks

  * effTyped(rhs, tpt): Tree
    - runs the EffTyper on rhs
    - needs to traverse the entire compilation unit: also for valdef/defdef that don't change
      type, the righthandside tree might still change type (Function, Select, Apply, ...). The
      trees need to be effTyped before effects are checked.
    - needs computeEffect: compute the effect Function bodies
        (TODO: for @rel effects? just skip skip it, the function rhs has the same @rel annotations as the enclosing method.
         but still effTyped the rhs?)

  * computeType(rhs): Type
    - simple function: returns the widened rhs.tpe; makes sure that effTyped(rhs, WildcardType?) has transformed rhs
    - USED FOR: inferring (refined) types

  * computeEffect(rhs): Elem
    - runs an EffectTraverser on rhs; makes sure that effTyped has transformed rhs before
    - USED FOR: inferring effects (of methods, but also of righthand-sides of Function trees), checkEffects

  * checkEffects
    - does not use computeEffect, no need to "make sure effTyped has run before", because once we get
      into checkEffects, everything has been effTyped. ==> no need to carry around rhsTyper etc.
    - verify effects of defdefs
    - verify types where not inferred (TODO: should this be done here? or by the effTyped)
    - verify overrides (similar to RefChecks)
    - not for @rel effects



3. expected type is used for
  - ptOrLub in if expressions (required!! not possible otherwise.)
  - implicit conversions
    - numeric widening (insert toChar / toInt / ...)
    - numeric narrowing (convert integer to Char / Byte / ...)
    - value discarding
    - view insertion
  - eta expansion of methods when a function is expected
  - "case" anonymous partial functions can only be type-checked with an expected type Function or PartialFunciton

  - local type inference: when type arguments are omitted (value selection, mehtod application),
  they are inferred to be "as specific as possible, while conforming to the expected type"


4. questions
  * transformed map and lazy types, identify symbols that are already effTyped
  * pass around rhsTyper, symbol (for which we are in the rhs), compilationUnit?

  * lattice-like things needed for @rel effects
    * fromAnnotation, toAnnotation
    * sub-effect: becasue the annotationChecker needs to implement "annotationsConform", which needs sub-effect.
      "annotationsConform" is used for lub/glb inference computations, using "specializesSym"
    * meet, join (for lub, glb)
    * NOT: sub-effecting.


