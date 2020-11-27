package archive.effect

import cats.effect.{ExitCode, IO, IOApp}

import scala.concurrent.duration.DurationInt

object Main extends IOApp {

  override def run(args: List[String]): IO[ExitCode] = {
    (IO.sleep(1.second) *> IO(println("Hello World"))).unsafeRunSync()
    IO(ExitCode.Success)
  }
}
