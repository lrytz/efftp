# To Do



- crash on this, rel(x) refers to the field
  class C(x: => Int) {
     @rel(x) type constructorEffect
     def foo(): Int @rel(x) = x
     lazy val v: Int @rel(x) = x
   }


- substitute variables that get out of scope in the result type
  => also need to do it in result type, but this is not possible in computeEffect, there
     we can't modify the types.. need another hook
  => maybe we don't need to do anything, the references to those local symbols can only be in
     effects of functions, and those effects are always obtained through computeEffect which
     already replaces the local vars? we'll see.

- substitute argument localities in result type when invoking a method
  => similar as above, for result type can't do it in computeEffect



- Pattern Matching: assign correct localities to pattern bound variables, make sure they are eliminated
  when they get out of scope



- disallow @mod(x) / @loc(x) where x is a repeated parameter


- when type checking a block, the expected assign effect includes local values (non-variables). they could be excluded


# Notes

- constructors should ususally be fresh, but (probably) not always. if a constructor assigns stale objects
  to local fields then the result is not fresh (and also has an effect)

