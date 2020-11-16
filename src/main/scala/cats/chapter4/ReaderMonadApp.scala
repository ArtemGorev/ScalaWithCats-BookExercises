package cats.chapter4

import cats.Id
import cats.data.{Kleisli, Reader}
import cats.syntax.applicative._

object ReaderMonadApp extends App {

  final case class Cat(name: String, favoriteFood: String)
  val catName: Reader[Cat, String] = Reader(cat => cat.name)

  println(catName.run(Cat("Tom", "meat")))
  println(catName.map(name => s"Hello $name").run(Cat("Tom", "meat")))
  private val f: Kleisli[Id, Cat, Int] = catName.map(name => 42)
  println(f.run(Cat("Tom", "meat")))

  val greetKitty: Reader[Cat, String] =
    catName.map(name => s"Hello ${name}")
  val feedKitty: Reader[Cat, String] =
    Reader(cat => s"Have a nice bowl of ${cat.favoriteFood}")

  val greenAndFeed: Reader[Cat, String] =
    for {
      greet <- greetKitty
      feed  <- feedKitty
    } yield s"$greet. $feed."

  println(greenAndFeed(Cat("Tom", "meat")))

  println("==========EXERCISE==========")

  final case class Db(
    usernames: Map[Int, String],
    passwords: Map[String, String]
  )

  type DbReader[A] = Reader[Db, A]

  def findUsername(userId: Int): DbReader[Option[String]] =
    new DbReader[Option[String]](db => db.usernames.get(userId))

  def checkPassword(username: String, password: String): DbReader[Boolean] =
    new DbReader[Boolean](db => db.passwords.get(username).contains(password))

  def checkLogin(userId: Int, password: String): DbReader[Boolean] = {
    for {
      username <- findUsername(userId)
      isValid <- username match {
        case Some(value) => checkPassword(value, password)
        case None        => false.pure[DbReader]
      }
    } yield isValid
  }

  val users = Map(
    1 -> "dade",
    2 -> "kate",
    3 -> "margo"
  )

  val passwords = Map(
    "dade"  -> "zerocool",
    "kate"  -> "acidburn",
    "margo" -> "secret"
  )

  val db = Db(users, passwords)

  println(checkLogin(1, "zerocool").run(db))
  println(checkLogin(4, "davinci").run(db))

}
