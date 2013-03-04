# Effects Plugin for Scala

## Building

The plugin builds with `sbt`. The `master` branch of the effects plugin builds against
Scala 2.10.1-RC2. I fyou want to have a plugin for Scala 2.11.0-SNAPSHOT, you can check
out the `scala-2.11` branch of this repository.


## Running the Tests

The tests for the plugin are in a separate sbt project: the reason is that we first need
to build the compiler plugin (done in sbt project `effects-plugin`) and then run the tests
using the plugin `.jar` file (sbt project `tests`).

Therefore, to run the tests, use the sbt command `tests/test`.


## Building against a local Scala version

In the past, the plugin was not able to build against a stock Scala release, it required
a patched Scala compiler. This is currently no longer the case, but we keep the notes below
just in case we'll ever need to use a custom compiler again.

The build file `project/ProjectBuild.scala` has some commented-out code with the tag
`#customScalaVersion`.

---

In order to build this project you need to build a build of the following branch

    https://github.com/lrytz/scala/tree/efftp

Then copy the contents of the folder `build/pack/lib` containing the Scala distribution
into the subfolder `lib/scala/lib` of this project.


To build `scalatest` against the custom scala version, check out the following branch

    svn checkout http://scalatest.googlecode.com/svn/branches/cmty2.10

Apply the patch `notes/local-scala.patch` and run `sbt package` in the scalatest project.
Copy the resulting jar file to `lib` folder of this project to make it available
(unmanaged).
