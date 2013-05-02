package scala.tools.nsc.effects

import scala.tools.nsc.Global
import scala.tools.nsc.plugins.PluginComponent
import scala.tools.nsc.transform.{TypingTransformers, Transform}

abstract class EffectChecker extends PluginComponent with Transform with TypeCheckerPlugin with TypeUtils {

  val global: Global

  val domain: EffectDomain {
    val global: EffectChecker.this.global.type
  }

  val phaseName: String = "effectchecker"
  val runsAfter: List[String] = List("extmethods")

  import global._
  import domain._
  import domain.lattice._

  def newTransformer(unit: CompilationUnit) = {
//    this.unit = unit
    new Checker //(unit)
  }

  // check effects for defdefs (and functions?) which don't have inferred effects
  // TODO: it seems we don't actually need that transformer - methods are already checked in addAnnotations
  // in the effect checker
  protected class Checker extends Transformer {
    override def transform(tree: Tree) = tree
  }
}

object EffectChecker {
  lazy val traceAnf = sys.props.isDefinedAt("efftp.traceAnf")

  // debugging stuff
  def printRes[T](res: T, msg: String = ""): T = {
    print(msg)
    println(res)
    res
  }
}