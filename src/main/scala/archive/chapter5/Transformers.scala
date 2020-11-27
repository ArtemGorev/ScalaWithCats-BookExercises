package archive.chapter5

import cats.data.EitherT
import cats.implicits.catsStdInstancesForFuture

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future}

object Transformers extends App {
  //    type Response[A] = Future[Either[String, A]]
  type Response[A] = EitherT[Future, String, A]

  val powerLevels = Map(
    "Jazz" -> 6,
    "Bumblebee" -> 8,
    "Hot ROD" -> 10
  )


  def getPowerLevel(autobot: String): Response[Int] = {
    powerLevels.get(autobot) match {
      case Some(value) => EitherT.right(Future(value))
      case None => EitherT.left(Future("Not found"))
    }
  }

  def canSpecialMove(ally1: String, ally2: String): Response[Boolean] =
    for {
      lvl1 <- getPowerLevel(ally1)
      lvl2 <- getPowerLevel(ally2)
    } yield (lvl1 + lvl2) > 15

  def tacticalReport(ally1: String, ally2: String): String = {
    val stack = canSpecialMove(ally1, ally2).value
    Await.result(stack, 1.second) match {
      case Left(msg) =>
        s"Comms error: $msg"
      case Right(true) =>
        s"$ally1 and $ally2 are ready to roll out!"
      case Right(false) =>
        s"$ally1 and $ally2 need a recharge."
    }
  }

  //  println(getPowerLevel("Jazz"))
  //  println(getPowerLevel("Bumblebee"))
  //  println(getPowerLevel("Hot ROD"))
//  println(getPowerLevel("Hot"))
  println(tacticalReport("Hot ROD", "Bumblebee"))
}
