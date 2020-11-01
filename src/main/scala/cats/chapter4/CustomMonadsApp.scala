package cats.chapter4

import cats.{Functor, Monad}

import scala.annotation.tailrec
import cats.syntax.flatMap._
import cats.instances.option._
import cats.syntax.functor._
import cats.syntax.monad._


object CustomMonadsApp extends App {

  implicit val optionMonad: Monad[Option] = new Monad[Option] {
    def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa.flatMap(f)

    @tailrec
    def tailRecM[A, B](a: A)(f: A => Option[Either[A, B]]): Option[B] =
      f(a) match {
        case None           => None
        case Some(Left(a))  => tailRecM(a)(f)
        case Some(Right(b)) => Some(b)
      }

    def pure[A](x: A): Option[A] = Some(x)
  }

  def retry[F[_]: Monad, A](start: A)(f: A => F[A]): F[A] =
    f(start).flatMap{ a =>
      retry(a)(f)
    }

  def retryTailRecM[F[_]: Monad, A](start: A)(f: A => F[A]): F[A] =
    Monad[F].tailRecM(start) { a =>
      f(a).map(a2 => Left(a2))
    }

  def retryM[F[_]: Monad, A](start: A)(f: A => F[A]): F[A] =
    start.iterateWhileM(f)(a => true)

  println(retry[Option, Int](100)(a => if (a == 0) None else Some(a - 1)))
  println(retryTailRecM[Option, Int](100000)(a => if(a == 0) None else Some(a - 1)))
  println(retryM[Option, Int](100000)(a => if(a == 0) None else Some(a - 1)))

  // ========EXERCISE========

  sealed trait Tree[+A]
  final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
  final case class Leaf[A](value: A)                        extends Tree[A]

  implicit val treeFunctor = new Functor[Tree] {
    def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = fa match {
      case Leaf(value)         => Leaf(f(value))
      case Branch(left, right) => Branch(map(left)(f), map(right)(f))
    }
  }

  implicit val treeMonad: Monad[Tree] = new Monad[Tree] {
    def flatMap[A, B](fa: Tree[A])(f: A => Tree[B]): Tree[B] = fa match {
      case Leaf(value)         => f(value)
      case Branch(left, right) => Branch(flatMap(left)(f), flatMap(right)(f))
    }

    @tailrec
    def tailRecM[A, B](a: A)(f: A => Tree[Either[A, B]]): Tree[B] =
      f(a) match {
        case Leaf(Left(value))  => tailRecM(value)(f)
        case Leaf(Right(value)) => Leaf(value)
        case _                  => throw new Exception()
      }

    def pure[A](x: A): Tree[A] = Leaf(x)
  }

  val tree = Branch(Leaf(1), Branch(Leaf(2), Branch(Leaf(3), Leaf(3))))

  println(retryTailRecM[Tree, Int](100000)(a => if(a == 0) Leaf(a) else Leaf(a - 1)))

//  println(treeMonad.tailRecM(tree)(x => Leaf(Right(x + 1))))



}
