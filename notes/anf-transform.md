# Todos

- the anf transform probably should not descend into Function trees - the plugin will anyway transform
  the body of Functions when necessary. same for method definitinos and lazy vals (check if we do).

- anf transform should conserve trees if their children are not changed. there's no reason to create
  a new ValDef (and type check it) if the rhs is already in ANF

- type checking the anf-transformed tree will call pluginsTyped of the plugin (and other functions of
  the plugin) ==> make sure that this has no unexepcted behavior
    - method and function bodies are anf-transformed again..!!!



- should probably erase the attributes (types and symbols) on the anf trees before type-checking them.
  the owner chain of the existing symbols can be invalidated.


- anf transform introduces local variable for setter argument (at least if the argument is a variable)

		{
		  var ei: C.this.E = e1;
		  val x$1$1: C.this.E = ei;
		  d.eLoc_=(x$1$1);
		  ei = e2
		}




## Use the correct typer

- "pluginsTyped" case DefDef    => typer is for outside the method, need to create inner one  ==> fixed
- "pluginsTyped" case Function  => same                                                       ==> fixed

- "pluginsTypeSig" case DefDef       => should be ok, we get the typer for the rhs as argument
- "pluginsTypeSig" case lazy ValDef  => should be ok

- "inferPrimaryConstrEff", constrBody           => probably ok
- "inferPrimaryConstrEff", field-initializers   => probably ok
- "inferPrimaryConstrEff", statement            => probably ok
-