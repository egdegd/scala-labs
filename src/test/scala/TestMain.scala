import java.nio.file.{Files, Path, Paths}
import java.util.Comparator

import cats.Id
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TestMain extends AnyFlatSpec with Matchers {
  implicit val fs: RealFileSystem[Id] = new RealFileSystem[Id]
  implicit val printer: ConsolePathPrinter[Id] = new ConsolePathPrinter[Id]
  val root: Path = Paths.get("./root")
  val program = new Program[Id, Path, Path]
  Files.createDirectory(root)
  program.run(root)

  val testDir: Path = root.resolve("test_dir")
  val fDir: Path = testDir.resolve("f")
  val bDir: Path = testDir.resolve("b")
  val foo: Path = fDir.resolve("foo")
  val bar: Path = bDir.resolve("bar")
  val baz: Path = bDir.resolve("baz")

  Files.exists(testDir) shouldBe true
  Files.exists(foo) shouldBe true
  Files.exists(bar) shouldBe true
  Files.exists(baz) shouldBe true

  Files.walk(root)
    .sorted(Comparator.reverseOrder())
    .forEach(file => Files.deleteIfExists(file))
}