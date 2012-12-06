# Effects Plugin for Scala

## Building

In order to build this project you need to build a build of the following branch

    https://github.com/lrytz/scala/tree/efftp

Then copy the contents of the folder `build/pack/lib` containing the Scala distribution
into the subfolder `lib/scala/lib` of this project.


To build `scalatest` against the custom scala version, check out the following branch

    svn checkout http://scalatest.googlecode.com/svn/branches/cmty2.10

Apply the patch `notes/local-scala.patch` and run `sbt package` in the scalatest project.
Copy the resulting jar file to `lib/scala/lib` of this project to make it available
(unmanaged).
