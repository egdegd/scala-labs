import cats.effect.concurrent.MVar
import cats.effect.{ExitCode, IO, IOApp, Resource}
import cats.syntax.all._

import scala.concurrent.duration._

object Lab10 extends IOApp {

  def runPrinter(mvar: MVar[IO, String]): Resource[IO, Unit] = {
    def rec: IO[Unit] = for {
      value <- mvar.take
      _ <- IO(println(value))
      _ <- rec
    } yield ()
    Resource.make(rec.start)(_.cancel.flatMap(_ => IO(println("Close printer")))).void
  }

  def runCounter(mvar: MVar[IO, String]): Resource[IO, Unit] = {
    def rec(counter: Long): IO[Unit] = for {
      _ <- mvar.put((counter).toString)
      _ <- IO.sleep(1.seconds)
      _ <- rec(counter + 1)
    } yield ()
    Resource.make(rec(0).start)(_.cancel.flatMap(_ => IO(println("Close counter")))).void
  }

  val gracefulShutdownProgram: Resource[IO, Unit] = for {
    mvar <- Resource.make(MVar.empty[IO, String])(_ => IO(print("Release")))
    _ <- runCounter(mvar)
    _ <- runPrinter(mvar)
  } yield ()


  override def run(args: List[String]): IO[ExitCode] =
    gracefulShutdownProgram.use(_ => IO.never)

}