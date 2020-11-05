package cats.chapter5

import cats.data.EitherT
import cats.implicits.{catsStdInstancesForFuture}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future}

object MonadTransformerExerciseApp extends App {

  type Response[A] = EitherT[Future, String, A]

  val powerLevels = Map(
    "Jazz"      -> 6,
    "Bumblebee" -> 8,
    "Hot Rod"   -> 10
  )

  def getPowerLevel(autobot: String): Response[Int] = {
     powerLevels.get(autobot) match {
       case Some(value) => EitherT.right(Future(value))
       case None        => EitherT.left(Future(s"autobot $autobot not found"))
     }
  }

  println(Await.result(getPowerLevel("Jazz").value, 1.second))
  println(Await.result(getPowerLevel("someautobot").value, 1.second))

  def canSpecialMove(ally1: String, ally2: String): Response[Boolean] = {
    powerLevels.get(ally1) match {
      case Some(power1) => powerLevels.get(ally2) match {
        case Some(power2) => EitherT.right(Future(power1 + power2 > 15))
        case None         => EitherT.left(Future(s"autobot $ally2 not found"))
      }
      case None         => EitherT.left(Future(s"autobot $ally1 not found"))
    }
  }

  def canSpecialMove2(ally1: String, ally2: String): Response[Boolean] =
    for {
      power1 <- getPowerLevel(ally1)
      power2 <- getPowerLevel(ally2)
    } yield power1 + power2 > 15

  println(Await.result(canSpecialMove("Hot Rod", "Bumblebee").value, 1.second))
  println(Await.result(canSpecialMove2("someautobot", "Bumblebee").value, 1.second))

  def tacticalReport(ally1: String, ally2: String): String =
    Await.result(canSpecialMove2(ally1, ally2).value, 1.second) match {
      case Right(true) => s"$ally1 and $ally2 are ready to roll out!"
      case Right(false) => s"$ally1 and $ally2 need a recharge"
      case Left(value) => value
    }

  println(tacticalReport("Jazz", "Bumblebee"))
  println(tacticalReport("Bumblebee", "Hot Rod"))
  println(tacticalReport("Jazz", "Ironhide"))
}
