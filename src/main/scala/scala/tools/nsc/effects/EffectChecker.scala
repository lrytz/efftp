package scala.tools.nsc.effects

import scala.tools.nsc.Global
import scala.tools.nsc.plugins.PluginComponent
import scala.tools.nsc.transform.InfoTransform

object EffectChecker {
  lazy val traceAnf = sys.props.isDefinedAt("efftp.traceAnf")

  // debugging stuff
  def printRes[T](res: T, msg: String = ""): T = {
    print(msg)
    println(res)
    res
  }
}

abstract class EffectsCleanup extends PluginComponent with InfoTransform {

  val global: Global

  val typeCheckerPlugin: TypeCheckerPlugin {
    val global: EffectsCleanup.this.global.type
  }

  val phaseName: String = "effectscleanup"
  val runsAfter: List[String] = List("refchecks")
  override val runsBefore: List[String] = List("selectiveanf")

  override protected def changesBaseClasses = false

  import global._

  def transformInfo(sym: Symbol, tpe: Type): Type = {
    typeCheckerPlugin.removeAllEffectAnnotations(tpe)
  }
  
  def newTransformer(unit: CompilationUnit) = new Transformer {
    override def transformUnit(unit: CompilationUnit) { }
  }
}
