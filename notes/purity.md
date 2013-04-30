# This to do for type system

OK
- selection has locality of the parameter (Ident; also Select? probably not)
- locality of a selection is the owner locality for local fields (Select)
- assigning has an effect (@mod for fields, @assign for local variables)

ToDo
- substitute variables that get out of scope in type, effect and locality (Block)
- substitute argument localities in effect, locality and result type when invoking a method



# More implementation specific points

- for every local variable we need to know its initial locality
    - need to attach it somehow to its symbol
    - when the variable gets out of scope (Block) the references to it need to be replaced by the initial locality
    - when re-assigning to the local variable, the locality of the new value has to conform to the initial locality

- the computed effect&locality for every tree should always be correct. we don't know where in the program that
  tree occurs. it might be the rhs of some function (eg a local one), then the computed effect&locality is what
  ends up on the annotations of that method

- a New tree should (probably) always type check as fresh - but the subsequent consructor invocation might make
  the object non-fresh

- constructors should ususally be fresh, but (probably) not always. if a constructo assigns stale objects
  to local fields then the result is not fresh (and also has an effect)

- expected effect is not really working out here
    - in a method where the expected effect is @mod(), inside the method there can be effects on local variables
    - the expected result locality should only be checked in the final result type, not in subexpressions











# Difficult Points

- Computing the localities might need additional support or changes in the framework. They don't
  propagate like effects:

    if (..) foo() else bar()    // locality is the union of both localities
    foo(); bar();               // locality of bar

  Effects always simply join, no matter if they happen in sequence or in alternative.

  ==> maybe the EffectDomain needs more hooks which are called by the TypeCheckerPlugin?


- Need to replace out-of-scope variables in types, effects and localities. I guess this has to be done in
  computeEffect when the tree is a block.



