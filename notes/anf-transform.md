# Todos

- should probably erase the attributes (types and symbols) on the anf trees before type-checking them.
  the owner chain of the existing symbols can be invalidated.


## Use the correct typer

- "pluginsTyped" case DefDef    => typer is for outside the method, need to create inner one  ==> fixed
- "pluginsTyped" case Function  => same                                                       ==> fixed

- "pluginsTypeSig" case DefDef       => should be ok, we get the typer for the rhs as argument
- "pluginsTypeSig" case lazy ValDef  => should be ok

- "inferPrimaryConstrEff", constrBody           => probably ok
- "inferPrimaryConstrEff", field-initializers   => probably ok
- "inferPrimaryConstrEff", statement            => probably ok
-