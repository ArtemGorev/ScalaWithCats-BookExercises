package cats.chapter7

import cats.Traverse
import cats.chapter7.TraverseApp.getUptime
import cats.instances.list._
import cats.instances.future._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future} // for Applicative

object TraverseCatsApp extends App {
  val hostnames = List(
    "alpha.example.com",
    "beta.example.com",
    "gamma.demo.com"
  )

  val totalUptime: Future[List[Int]] = Traverse[List].traverse(hostnames)(getUptime)
  println(Await.result(totalUptime, 1.second))
  val numbers                     = List(Future(1), Future(2), Future(3))
  val numbers2: Future[List[Int]] = Traverse[List].sequence(numbers)
  println(Await.result(numbers2, 1.second))

}
