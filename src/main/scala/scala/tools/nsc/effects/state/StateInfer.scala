package scala.tools.nsc.effects
package state

trait StateInfer { this: StateDomain =>

  import global._
  import lattice._

  override def computeEffectImpl(tree: Tree, ctx: EffectContext): Effect = {
    ???
  }


  /**
   * The locality of a selected symbol, i.e. the locality of a Select or Ident tree.
   *
   *  - if the symbol is a parameter or a local value / variable defined in the current
   *    or an outer method, then look up the locality in the environment
   *
   *  - if the symbol is anything else, AnyLoc
   */

/*
  def selectedLocality(sym: Symbol, env: Env, ctx: Context): Locality = {
    val res = if((sym.isParameter || sym.isLocal) && definedInEnclMethod(sym, ctx)) {
      env.lookup(SymLoc(sym))
    } else {
      AnyLoc
    }
    if (settings.debug.value) // @DEBUG
      println("locality of selected "+ sym +" is "+ res)
    res
  }
*/

}
