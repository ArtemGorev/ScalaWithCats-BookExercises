package archive.chapter2

import cats.data.Reader

final case class Db(
                     usernames: Map[Int, String],
                     passwords: Map[String, String]
                   )


object ReaderMonad extends App {
  type DbReader[A] = Reader[Db, A]

  def findUsername(userId: Int): DbReader[Option[String]] =
    Reader(db => db.usernames.get(userId))

  def checkPassword(
                     username: String,
                     password: String): DbReader[Boolean] =
    Reader(db => db.passwords.get(username).contains(password))

  def checkLogin(
                  userId: Int,
                  password: String
                ): DbReader[Boolean] = {
    for {
      username <- findUsername(userId)
      ans <- checkPassword(username.getOrElse(""), password)
    } yield ans
  }

  val users = Map(
    1 -> "dade",
    2 -> "kate",
    3 -> "margo"
  )

  val passwords = Map(
    "dade" -> "zerocool",
    "kate" -> "acidburn",
    "margo" -> "secret"
  )
  val db = Db(users, passwords)
  println(checkLogin(1, "zerocool").run(db))
  println(checkLogin(4, "davinci").run(db))


  println(Right("Hello")
    .flatMap(x => Right(" World")
        .map { y => val z = x ++ y; (y, z) }
        .map { case (y, z) => z }
    ))
}
