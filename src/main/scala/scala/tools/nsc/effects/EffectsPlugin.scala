package scala.tools.nsc.effects

import scala.tools.nsc.Global
import scala.tools.nsc.plugins.Plugin

class EffectsPlugin(val global: Global) extends Plugin {
  /** The name of this plugin. */
  val name = "effects"

  /** A short description of the plugin */
  val description = "tracks side effects"

  val domain = new io.IODomain {
    val global: EffectsPlugin.this.global.type = EffectsPlugin.this.global
  }
  val effectChecker = new {
    val global: EffectsPlugin.this.global.type = EffectsPlugin.this.global
    val domain = EffectsPlugin.this.domain
  } with EffectChecker

  /**
   * The compiler components that will be applied when running this plugin
   */
  val components = List(effectChecker)
}
