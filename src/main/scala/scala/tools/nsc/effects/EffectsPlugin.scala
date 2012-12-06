package scala.tools.nsc.effects

import scala.tools.nsc.Global
import scala.tools.nsc.plugins.Plugin

class EffectsPlugin(val global: Global) extends Plugin {
  /** The name of this plugin. */
  val name = "effects"

  /** A short description of the plugin */
  val description = "tracks side effects"

  /** A description of the plugin's options */
/*
  override val optionsHelp =
    Some("  -P:"+ name +":log-exc        show the inferred exceptions of methods\n"+
         "  -P:"+ name +":log-pcall      show the inferred parameter calls of methods")
*/

  /** parsing of plugin options */
/*
  override def processOptions(options: List[String], error: String => Unit) {
    val rest = new collection.mutable.ListBuffer[String]()
    for (o <- options) {
      if (o == "log-exc")
        EffectsPlugin.logExc = true
      else if (o == "log-pcall")
        EffectsPlugin.logPcall = true
      else
        rest += o
    }
    super.processOptions(rest.toList, error)
  }
*/

/*
  val relTyper = new scala.tools.nsc.effectsnew.relative.RelTyper(global)
  val relInferencer = new {
    val annTyper = relTyper
  } with scala.tools.nsc.effectsnew.relative.RelInferencer


  val simpleChecker = new simple.SimpleChecker(global)
  val simpleInferencer = new {
    val checker = simpleChecker
  } with EffectInferencer[simple.SimpleLattice] {
    val runsAfter = List("pcChecker")
    val phaseName = "simpleInferencer"
  }
*/

  /**
   * The compiler components that will be applied when running this plugin
   */
  val components = List()
}
/*
object EffectsPlugin {
  var logPcall = false
  var logExc = false
}
*/