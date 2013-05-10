# Effects Plugin for Scala

This repo has the source of a Scala compiler plugin for effect checking. For documentation
refer to the [Wiki on Gihub](https://github.com/lrytz/efftp/wiki).

## Scala Version

The `master` branch of this repository builds against Scala 2.10.1.

In the upcoming Scala 2.11 (the `master` branch of the the `scala/scala` repository), templates and constructors have a slightly different tree representation. The `scala-2.11` should take that into account (it is not up to date though, the main branch might not merge cleanly into it, and it's not tested)

## Building

Just run `sbt package`. The resulting `target/scala-2.10/effects-plugin_2.10-0.1-SNAPSHOT.jar` file is a compiler plugin that can be used with the Scala 2.10.1 compiler (the Wiki explains how).

## Running the Tests

The tests for the plugin are in a separate sbt project: the reason is that we first need
to build the compiler plugin (done in sbt project `effects-plugin`) and then run the tests
using the plugin `.jar` file (sbt project `tests`).

Therefore, use the sbt command `tests/test` to run the tests. For details on the testing framework check the Wiki.

## Screenshot

![Eclipse Screenshot](https://github.com/lrytz/efftp/wiki/images/ee.png)


## Compiler Output

```
tsf-444-wpa-1-227:src luc$ ~/scala/scala-2.10.1/bin/scalac -P:effects:domains:purity:exceptions:io -Xplugin:/Users/luc/scala/efftp/target/scala-2.10/effects-plugin_2.10-0.1-SNAPSHOT.jar -cp /Users/luc/scala/efftp/target/scala-2.10/effects-plugin_2.10-0.1-SNAPSHOT.jar T.scala 
T.scala:10: error: effect type mismatch;
 found   : @io
 required: @noIo
    println()
           ^
T.scala:18: error: effect type mismatch;
 found   : @mod(C.this) @loc(any)
 required: @mod(x$1$2) @assign(x$1$2,any) @loc(any)
@mod(C.this) does not conform to @mod(x$1$2)
  def incBad(): Unit @mod() = x = x + 1
                                ^
T.scala:24: error: effect type mismatch;
 found   : @mod() @loc() @throws[C.this.E1] @noIo
 required: @mod(any) @assign(any) @loc(any) @throws[Nothing] @io
@throws[C.this.E1] does not conform to @throws[Nothing]
  def bad(): Int @throws[Nothing] = throw new E1
                                    ^
three errors found
```


## More Examples

Have a look at the [tests](https://github.com/lrytz/efftp/tree/master/tests/src/test/resources/scala/tools/nsc/effects).
The most enlightening test is probably [the one on collections](https://github.com/lrytz/efftp/blob/master/tests/src/test/resources/scala/tools/nsc/effects/multi/Colls-files/colls.scala).
