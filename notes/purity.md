# This to do for type system

OK
- selection has locality of the parameter (Ident; also Select? probably not)
- locality of a selection is the owner locality for local fields (Select)
- assigning has an effect (@mod for fields, @assign for local variables)

ToDo
- substitute variables that get out of scope in effect and locality (Block)
  => also need to do it in result type, but this is not possible in computeEffect, there
     we can't modify the types.. need another hook
  => maybe we don't need to do anything, the references to those local symbols can only be in
     effects of functions, and those effects are always obtained through computeEffect which
     already replaces the local vars? we'll see.



- substitute argument localities in effect, locality and result type when invoking a method
  => similar as above, for result type can't do it in computeEffect



# More implementation specific points

- integrate ANF transform in all places where necessary

- expected effect is not really working out here
    - in a method where the expected effect is @mod(), inside the method there can be effects on local variables
    - the expected result locality should only be checked in the final result type, not in subexpressions

- the computed effect&locality for every tree should always be correct. we don't know where in the program that
  tree occurs. it might be the rhs of some function (eg a local one), then the computed effect&locality is what
  ends up on the annotations of that method

- a New tree should (probably) always type check as fresh - but the subsequent consructor invocation might make
  the object non-fresh

- constructors should ususally be fresh, but (probably) not always. if a constructor assigns stale objects
  to local fields then the result is not fresh (and also has an effect)








# Difficult Points

- Computing the localities might need additional support or changes in the framework. They don't
  propagate like effects:

    if (..) foo() else bar()    // locality is the union of both localities
    foo(); bar();               // locality of bar

  Effects always simply join, no matter if they happen in sequence or in alternative.

  ==> maybe the EffectDomain needs more hooks which are called by the TypeCheckerPlugin?


- Need to replace out-of-scope variables in types, effects and localities. I guess this has to be done in
  computeEffect when the tree is a block.



# Random Todos

- disallow @mod(x) / @loc(x) where x is a repeated parameter


