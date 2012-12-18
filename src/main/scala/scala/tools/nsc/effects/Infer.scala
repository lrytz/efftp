package scala.tools.nsc.effects

trait Infer { self: EffectDomain =>
  import global._
  import lattice._

  /**
   * Compute the effect of some tree. Needs enclFun for effect polymorphism
   */
  final def inferEffect(tree: Tree, enclFun: Symbol): Effect = {
    val infer = new EffectInfer(enclFun)
    infer.traverse(tree)
    infer.effect
  }

  /**
   * TODO: rename
   *
   * compute the effect of a tree. called by the traverser on every sub-tree.
   * overriding this method allows concrete domains to specify the effect of
   * certain trees.
   */
  def inferEff(tree: Tree, enclFun: Symbol, set: Effect => Unit, continue: => Unit) {
    tree match {
      case Apply(fun, args) =>
        val funEff   = inferEffect(fun, enclFun)
        val argsEffs = args map (inferEffect(_, enclFun))

        val latent: Effect = bottom
        val e = (funEff u (argsEffs :\ bottom)(_ u _) u latent)
        set(e)

      case _ =>
        continue
    }
  }

  private class EffectInfer(enclFun: Symbol) extends Traverser {
    var effect: Effect = bottom

    override def traverse(tree: Tree) {
      inferEff(tree, enclFun, e => {effect = effect u e}, super.traverse(tree))
    }
  }
}
