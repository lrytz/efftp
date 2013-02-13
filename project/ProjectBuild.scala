import sbt._
import Keys._

object ProjectBuild extends Build {

  val scalaVersionString = "scala-effects"
  val scalaLibraryModuleString = "org.scala-lang:scala-library:"+ scalaVersionString

  def scalaHomeDir(base: File): Option[File] = Some(base / "lib" / "scala")

  // settings valid for both projects (the plugin and the tests)
  val sharedSettings = Seq (
    // to be clear we're not using a release
    scalaVersion := scalaVersionString,

    // remove scala-library dependency; otherwise sbt tries to download the scala lib version
    // "scala-effects", which fails.
    libraryDependencies ~= { (deps: Seq[ModuleID]) =>
      deps.filterNot(_.toString.startsWith(scalaLibraryModuleString))
    },

    libraryDependencies += "org.springframework" % "spring-core" % "3.2.1.RELEASE" % "test",

    // add the jars from the scala distro to the classpath
    (unmanagedJars in Compile) <<= (unmanagedJars in Compile, scalaHome) map { (jars, homeDir) =>
      val scalaJars = homeDir.get / "lib" * "*.jar"
      jars ++ scalaJars.get.map(Attributed.blank(_))
    }
  )


  lazy val pluginProject: Project = Project(id = "plugin", base = file(".")) settings (
    name := "effects-plugin",

    // scalaHome is usually "None" - if it's Some, it defines the compiler that sbt uses
    scalaHome <<= baseDirectory { scalaHomeDir }
  ) settings (sharedSettings: _*)


// print trees:
// sbt> set (scalacOptions in testsProject) += "-Xprint:typer"
// sbt> set (scalacOptions in testsProject) += "-Yshow-trees"

  lazy val testsProject = Project(id = "tests", base = file("tests")) settings (
    name := "effects-tests",

    unmanagedBase <<= (unmanagedBase in pluginProject),

    scalaHome <<= (baseDirectory in pluginProject) { scalaHomeDir },

    (test in Test) <<= (test in Test).dependsOn(test in (pluginProject, Test)),

    // scala compiler seems to crash on diff_match_patch.java - a problem of the effects plugin?
    compileOrder in Test := CompileOrder.JavaThenScala,

    fork := true,

    javaOptions <++= (packageBin in (pluginProject, Compile)) map { pluginJar => Seq(
      "-DeffectsPlugin.jarFile="+ pluginJar.getAbsolutePath,
      "-agentlib:jdwp=transport=dt_socket,server=y,suspend=n,address=5006"
    )},

   scalacOptions <++= (packageBin in (pluginProject, Compile)) map { pluginJar => Seq(
     "-Xplugin:"+ pluginJar.getAbsolutePath,
     "-P:effects:domains:io"
   )}

  ) settings (sharedSettings: _*) dependsOn (pluginProject)
}
