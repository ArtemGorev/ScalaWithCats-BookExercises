package archive.chapter7

import cats.Applicative
import cats.instances.vector._
import cats.syntax.apply._
import cats.syntax.applicative._

object Folding extends App {
  def listTraverse[F[_]: Applicative, A, B]
  (list: List[A])(func: A => F[B]): F[List[B]] =
    list.foldLeft(List.empty[B].pure[F]) { (accum, item) =>
      (accum, func(item)).mapN(_ :+ _)
    }

  def listSequence[F[_]: Applicative, B]
  (list: List[F[B]]): F[List[B]] =
    listTraverse(list)(identity)

  println(listSequence(List(Vector(1, 2), Vector(3, 4))))
}
