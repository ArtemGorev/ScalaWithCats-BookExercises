package cats.chapter8

import scala.concurrent.Future
import cats.{Applicative, Id}
import cats.instances.future._
import cats.instances.list._
import cats.syntax.traverse._
import cats.syntax.applicative._
import cats.syntax.functor._

import scala.concurrent.ExecutionContext.Implicits.global

object TestingAsyncCodeApp extends App {

  trait UptimeClient[F[_]] {
    def getUptime(hostname: String): F[Int]
  }

  class UptimeService[K[_] : Applicative](client: UptimeClient[K]) {
    def getTotalUptime(hostnames: List[String]): K[Int] = {
      hostnames.traverse(client.getUptime).map(_.sum)
    }
  }

  class RealUptimeClient(hosts: Map[String, Int]) extends UptimeClient[Future] {
    override def getUptime(hostname: String): Future[Int] = hosts.getOrElse(hostname, 0).pure[Future]
  }

  class TestUptimeClient(hosts: Map[String, Int]) extends UptimeClient[Id] {
    override def getUptime(hostname: String): Id[Int] = hosts.getOrElse(hostname, 0).pure[Id]
  }

  def testTotalUptime(): Unit = {
    val hosts    = Map("host1" -> 10, "host2" -> 6)
    val client   = new TestUptimeClient(hosts)
    val service  = new UptimeService(client)
    val actual   = service.getTotalUptime(hosts.keys.toList)
    val expected = hosts.values.sum
    println(actual == expected)
    assert(actual == expected)
  }

  testTotalUptime()
}
