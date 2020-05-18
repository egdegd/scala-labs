import java.nio.file.{Files, Path, Paths, StandardCopyOption}

import cats.instances.list._
import cats.syntax.all._
import cats.{Applicative, Id, Monad}

import scala.collection.JavaConverters._
import scala.language.higherKinds


trait MkDir[F[_], Dir] {
  def mkDir(dir: Dir, name: String): F[Dir]
}

trait MkFile[F[_], Dir, File] {
  def mkFile(dir: Dir, name: String): F[File]
}

trait GetFiles[F[_], Dir, File] {
  def getFiles(dir: Dir): F[List[File]]
}

trait GetName[F[_], File] {
  def getName(file: File): F[String]
}

trait MoveFile[F[_], Dir, File] {
  def moveFile(file: File, dir: Dir): F[File]
}

trait Printer[F[_], File] {
  def printName(file: File): F[Unit]
}

class Program[F[_], Dir, File](implicit
                               F: Monad[F],
                               mkDir: MkDir[F, Dir],
                               mkFile: MkFile[F, Dir, File],
                               getFiles: GetFiles[F, Dir, File],
                               getFileName: GetName[F, File],
                               moveFile: MoveFile[F, Dir, File],
                               printer: Printer[F, File]) {
  def run(dir: Dir): F[Unit] = for {
    testDir <- mkDir.mkDir(dir, "test_dir")
    _ <- mkFile.mkFile(testDir, "foo")
    _ <- mkFile.mkFile(testDir, "bar")
    _ <- mkFile.mkFile(testDir, "baz")
    files <- getFiles.getFiles(testDir)
    _ <- files.traverse(file => printer.printName(file))
    names <- files.traverse(file => getFileName.getName(file))
    letters = names.map(_.head)
    dirs <- letters.traverse(ch => mkDir.mkDir(testDir, ch.toString))
    _ <- files.zip(dirs).traverse(pair => moveFile.moveFile(pair._1, pair._2))
  } yield ()
}

class RealFileSystem[F[_] : Applicative] extends MkDir[F, Path] with MkFile[F, Path, Path]
  with GetFiles[F, Path, Path] with GetName[F, Path] with MoveFile[F, Path, Path] {
  override def mkDir(dir: Path, name: String): F[Path] = {
    if (Files.exists(dir.resolve(name))) {
      dir.resolve(name).pure[F]
    } else {
      Files.createDirectories(dir.resolve(name)).pure[F]
    }
  }

  override def mkFile(dir: Path, name: String): F[Path] = {
    if (Files.exists(dir.resolve(name))) {
      dir.resolve(name).pure[F]
    } else {
      Files.createFile(dir.resolve(name)).pure[F]
    }
  }

  override def getFiles(dir: Path): F[List[Path]] =
    Files.list(dir).filter(Files.isRegularFile(_)).iterator().asScala.toList.pure[F]

  override def getName(file: Path): F[String] = file.getFileName.toString.pure[F]

  override def moveFile(file: Path, dir: Path): F[Path] =  Files.move(file, dir.resolve(file.getFileName), StandardCopyOption.REPLACE_EXISTING).pure[F]
}

class ConsolePathPrinter[F[_] : Applicative] extends Printer[F, Path] {
  override def printName(file: Path): F[Unit] = println(file.getFileName.toString).pure[F]
}

object TypeClasses {
  def main(args: Array[String]): Unit = {
    implicit val fs: RealFileSystem[Id] = new RealFileSystem[Id]
    implicit val printer: ConsolePathPrinter[Id] = new ConsolePathPrinter[Id]
    val program = new Program[Id, Path, Path]
    program.run(Paths.get("."))
  }
}

