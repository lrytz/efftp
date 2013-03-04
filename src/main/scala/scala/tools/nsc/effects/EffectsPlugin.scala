package scala.tools.nsc.effects

import scala.tools.nsc.Global
import scala.tools.nsc.plugins.Plugin

class EffectsPlugin(val global: Global) extends Plugin {
  /** The name of this plugin. */
  val name = "effects"

  /** A short description of the plugin */
  val description = "tracks side effects"

  /**
   * Do nothing, this method is invoked too late.
   *
   * The compiler accesses the `components` filed of the compiler plugin before calling
   * `processOptions`. Therefore the `settings` object is not yet initialized when the
   * component (i.e. the `effectChecker`, and the `domain`) are computed.
   *
   * Instead of processing options here, the constructor of `settings` reads them directly
   * from `global`.
   */
  override def processOptions(options: List[String], error: String => Unit) {
    ()
  }

  val domain: EffectDomain { val global: EffectsPlugin.this.global.type } = {
    settings.domains match {
      case List("io") =>
        new io.IODomain {
          val global: EffectsPlugin.this.global.type = EffectsPlugin.this.global
        }

      case List("exceptions") =>
        new exceptions.ExceptionsDomain {
          val global: EffectsPlugin.this.global.type = EffectsPlugin.this.global
        }

      case List("state") =>
        new state.StateDomain {
          val global: EffectsPlugin.this.global.type = EffectsPlugin.this.global
        }

      case ds =>
        global.abort(s"Unknown domains: $ds")
    }
  }
  val effectChecker = new {
    val global: EffectsPlugin.this.global.type = EffectsPlugin.this.global
    val domain = EffectsPlugin.this.domain
  } with EffectChecker

  /**
   * The compiler components that will be applied when running this plugin
   */
  val components = List(effectChecker)



  object settings {
    var domains: List[String] = Nil
    val domainsStr = "domains:"

    process(globalOptions, s => global.abort(s))

    /**
     * Read the options for this compiler plugin - see documentation on `processOptions`
     * above.
     */
    def globalOptions = {
      val namec = name+":"
      val opts = global.settings.pluginOptions.value.filter(_ startsWith namec)
      opts.map(_ stripPrefix namec)
    }

    def process(opts: List[String], error: String => Unit) {
      opts match {
        case x :: xs if x.startsWith(domainsStr) =>
          if (domains.nonEmpty) {
            error("Multiple 'domain' settings")
          } else {
            domains = x.drop(domainsStr.length).split(":").toList
          }
          process(xs, error)

        case x :: xs =>
          error(s"Unknown setting: $x")

        case Nil =>
          ()
      }
    }
  }
}

