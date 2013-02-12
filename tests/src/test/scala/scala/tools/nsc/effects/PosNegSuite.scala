package scala.tools.nsc.effects

import org.scalatest.FunSuite

import scala.annotation.effects._

import java.net.URI
import java.io.{PrintWriter, File}
import org.springframework.core.io.support.PathMatchingResourcePatternResolver

/**
 * A ScalaTest suite for testing the compiler.
 */
abstract class PosNegSuite extends FunSuite {

  /**
   * If the name of a test is in this list, a `.check` file will be created (existing ones are
   * overwritten) containing the compiler output.
   *
   * Therefore, the easiest way to get a `neg` test running is to put its source code in place,
   * add its name to the `updateCheck` list and run the test suite.
   */
  val updateCheck: List[String] = List()

  /**
   * Runs the tests. This method is invoked at the end of this constructor.
   */
  def runPosNeg() {
    for (testFile <- tests) {
      test(testFile.name) {
        testFile match {
          case t if updateCheck.contains(t.name) =>
            val (_, log) = compiler.compile(readFile(t.src))
            testFile.updateCheck(log)

          case PosTest(name, src) =>
            val (res, log) = compiler.compile(readFile(src))
            lazy val msg = s"compiler output:\n$log\n." // dot at the end, otherwise ScalaTest removes newline
            assert(res && log.isEmpty, msg)

          case NegTest(name, src, check) =>
            val (res, log) = compiler.compile(readFile(src))
            val checkStr = readFile(check)
            lazy val msg = s"expected:\n$checkStr\n\nfound:\n$log\n."
            assert(!res && log == checkStr, msg)
        }
      }
    }
  }


  val compiler = new EffectsCompiler

  val sep = sys.props("file.separator")

  val testsSubdir = getClass.getCanonicalName.replace(".", sep)

  val tests: List[Test] = {
    val resolver = new PathMatchingResourcePatternResolver(getClass.getClassLoader)
    val resources = resolver.getResources(s"${testsSubdir}${sep}*").toList.map(_.getURI)
    testsFromURIs(resources)
  }

  private def testsFromURIs(resources: List[URI]): List[Test] = {
    def isScala(uri: URI) = uri.toString.endsWith(".scala")
    def isCheck(uri: URI) = uri.toString.endsWith(".check")

    def nameOf(uri: URI) = uri.toString.split("/").last.dropRight(6)

    def isCheckFor(name: String, uri: URI) = uri.toString.endsWith(name+".check")
    def isScalaFor(name: String, uri: URI) = uri.toString.endsWith(name+".scala")

    resources match {
      case x :: xs if isScala(x) =>
        val name = nameOf(x)
        val (check, others) = xs.partition(isCheckFor(name, _))
        val test = check match {
          case c :: _ => NegTest(name, x, c)
          case _      => PosTest(name, x)
        }
        test :: testsFromURIs(others)

      case x :: xs if isCheck(x) =>
        val name = nameOf(x)
        val (scala, others) = xs.partition(isScalaFor(name, _))
        val test = scala match {
          case s :: _ => NegTest(name, s, x)
          case _      => error("no scala file found for check file "+ x)
        }
        test :: testsFromURIs(others)

      case x :: xs =>
        testsFromURIs(xs)

      case Nil =>
        Nil
    }
  }


  trait Test {
    def name: String
    def src: URI

    val filePrefix = "file:"
    def updateCheck(content: String) {
      val srcStr = src.toString
      if (srcStr startsWith filePrefix) {
        val srcFile = new File(srcStr stripPrefix filePrefix)
        assert(srcFile.exists, srcFile)
        val checkFile = checkFileForTestSource(srcFile, name)
        val w = new PrintWriter(checkFile)
        w.write(content)
        w.close()
      } else {
        error(s"Cannot write .check file, source is not a plain file: $srcStr")
      }
    }
  }
  case class PosTest(name: String, src: URI) extends Test
  case class NegTest(name: String, src: URI, check: URI) extends Test


  /**
   * Example:
   *  /.../efftp/tests/target/scala-scala-effects/test-classes/scala/tools/nsc/effects/io/ClassesSuite/constructorInferenceNeg.scala
   *  /.../efftp/tests/src/test/resources/scala/tools/nsc/effects/io/ClassesSuite/constructorInference.check
   */
  private def checkFileForTestSource(testSrc: File, name: String): File = {
    def parent(file: File, n: Int): File = if (n > 0) parent(file.getParentFile, n-1) else file
    def subPath(base: String, subs: String*) =
      base + sep + subs.mkString(sep)

    val targetTestsDir = testSrc.getParentFile.getAbsolutePath
    val testClassesDir = new File(targetTestsDir.stripSuffix(testsSubdir))
    val testsDir = parent(testClassesDir, 3).getAbsolutePath

    val checkFile = subPath(testsDir, "src", "test", "resources") + sep + testsSubdir + sep + name + ".check"
    new File(checkFile)
  }

  def readFile(uri: URI) = scala.io.Source.fromURI(uri).mkString

  runPosNeg()
}
