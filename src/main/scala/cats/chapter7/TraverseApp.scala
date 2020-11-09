package cats.chapter7

import cats.Applicative
import cats.implicits.catsSyntaxApplicativeId

import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import cats.syntax.apply._
import cats.instances.future._

object TraverseApp extends App {
  val hostnames = List(
    "alpha.example.com",
    "beta.example.com",
    "gamma.demo.com"
  )
  def getUptime(hostname: String): Future[Int] =
    Future(hostname.length * 60)

  val allUptimes: Future[List[Int]] =
    hostnames.foldLeft(Future(List.empty[Int])) { (accum, host) =>
      val uptime = getUptime(host)
      for {
        accum  <- accum
        uptime <- uptime
      } yield accum :+ uptime
    }
  println(Await.result(allUptimes, 1.second))

  val allUptimes2: Future[List[Int]] =
    Future.traverse(hostnames)(getUptime)
  println(Await.result(allUptimes2, 1.second))

  println(Future(List.empty[Int]))
  println(List.empty[Int].pure[Future])

  def oldCombine(accum: Future[List[Int]], host: String): Future[List[Int]] = {
    accum.flatMap(accum => getUptime(host).map(uptime => accum :+ uptime))
  }

  def newCombine(accum: Future[List[Int]], host: String): Future[List[Int]] =
    (accum, getUptime(host)).mapN(_ :+ _)

  def listTraverse[F[_]: Applicative, A, B](list: List[A])(func: A => F[B]): F[List[B]] =
    list.foldLeft(List.empty[B].pure[F]) { (accum, item) => (accum, func(item)).mapN(_ :+ _) }

  def listSequence[F[_]: Applicative, B](list: List[F[B]]): F[List[B]] =
    listTraverse(list)(identity)

  println(Await.result(listTraverse(List(1, 2, 3))((a: Int) => a.toString.pure[Future]), 1.second))

  val totalUptime = listTraverse(hostnames)(getUptime)
  println(Await.result(totalUptime, 1.second))

  println(Runtime.getRuntime.availableProcessors)
}
