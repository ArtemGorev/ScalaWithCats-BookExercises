package cats.chapter3

import cats.Contravariant
import cats.Show
import cats.Monoid
import cats.instances.string._
import cats.syntax.contravariant._
import cats.syntax.invariant._ // for imap
import cats.syntax.semigroup._

object ContravariantCatsApp extends App {

  val showString: Show[String] = Show[String]
  val showSymbol: Show[Symbol] = Contravariant[Show].contramap(showString)((sym: Symbol) => s"'${sym.name}")
  println(showSymbol.show(Symbol("dave")))
  println(showString
    .contramap[Symbol]((sym: Symbol) => s"'${sym.name}")
    .show(Symbol("dave")))

  implicit val symbolMonoid: Monoid[Symbol] =
    Monoid[String].imap(Symbol.apply)(_.name)

  println(Monoid[Symbol].empty)
  println(Symbol("a") |+| Symbol("few") |+| Symbol("words"))

}
