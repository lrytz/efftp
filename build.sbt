name := "effects-plugin"

// scalaHome is usually "None" - if it's Some, it defines the compiler that sbt uses
scalaHome in ThisBuild <<= baseDirectory { base =>
  Some(base / "lib/scala")
}

// to be clear we're not using a release
scalaVersion := "scala-effects"

// remove scala-library dependency; otherwise sbt tries to download the scala lib version "scala-effects",
// which fails. instead, we add the libraries as unmanaged (see below)
libraryDependencies := Seq()

// use the distribution's lib directory for unmanaged libs
unmanagedBase <<= baseDirectory { base => base / "lib/scala/lib" }
