package scala.tools.nsc.effects

import java.io.{PrintWriter, StringWriter}
import tools.nsc.util.CommandLineParser
import tools.nsc.{Global, Settings}
import tools.nsc.reporters.{ConsoleReporter, Reporter}
import reflect.internal.util.BatchSourceFile

class EffectsCompiler(domains: List[String]) {

  def extraSettings: String = ""

  private val effectsPluginJar = sys.props("effectsPlugin.jarFile")
  private def effectTestSettings = List(
    "-usejavacp",
    "-Ystop-after:refchecks",
    s"-Xplugin:$effectsPluginJar",
    s"-P:effects:domains:${domains.mkString(":")}"
  ).mkString(" ")

  private def allSettings = s"$effectTestSettings $extraSettings"

  // a custom Settings object
  def newSettings() = {
    val settings = CommandLineParser.tokenize(allSettings)
    val s = new Settings
    s processArguments (settings, true)
    s
  }

  private var compilerLogWriter = new StringWriter()
  def newReporter(settings: Settings): Reporter = new ConsoleReporter(settings, Console.in, new PrintWriter(compilerLogWriter))

  val compiler: Global = {
    val settings = newSettings()
    new Global(settings, newReporter(settings))
  }



  def compile(sourceCode: String): (Boolean, String) = {
    val file = new BatchSourceFile("newSource", sourceCode)
    compiler.reporter.reset()
    compilerLogWriter.getBuffer.setLength(0)

    val run = new compiler.Run()
    run.compileSources(List(file))
    compiler.reporter.flush()

    (!compiler.reporter.hasErrors, compilerLogWriter.toString())
  }
}
