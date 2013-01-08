package scala.tools.nsc.effects

import tools.nsc.Global
import tools.nsc.plugins.PluginComponent
import tools.nsc.transform.{TypingTransformers, Transform}

abstract class EffectChecker extends PluginComponent with Transform with AnnotChecker with TypeUtils {

  val global: Global

  val domain: EffectDomain {
    val global: EffectChecker.this.global.type
  }

  val phaseName: String = "effectchecker"
  val runsAfter: List[String] = List("superaccessors")

  import global._
  import domain._
  import domain.lattice._

  def newTransformer(unit: CompilationUnit) = {
//    this.unit = unit
    new Checker //(unit)
  }

  // check effects for defdefs (and functions?) which don't have inferred effects
  protected class Checker extends Transformer {
    override def transform(tree: Tree) = tree
  }

  // debugging stuff
  def printRes[T](res: T, msg: String = ""): T = {
    if (!msg.isEmpty) {
      println(msg)
    }
    println(res)
    res
  }
}
