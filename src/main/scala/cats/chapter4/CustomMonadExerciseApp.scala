package cats.chapter4

import cats.Monad

import scala.annotation.tailrec
import cats.syntax.either._
import cats.instances.either._

object CustomMonadExerciseApp extends App {

  sealed trait Tree[+A]
  final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
  final case class Leaf[A](value: A)                        extends Tree[A]

  implicit val treeMonad = new Monad[Tree] {
    def pure[A](value: A): Tree[A] =
      Leaf(value)

    def flatMap[A, B](tree: Tree[A])(func: A => Tree[B]): Tree[B] =
      tree match {
        case Branch(l, r) => Branch(flatMap(l)(func), flatMap(r)(func))
        case Leaf(value)  => func(value)
      }

    def tailRecM[A, B](arg: A)(func: A => Tree[Either[A, B]]): Tree[B] = {
      @tailrec
      def loop(open: List[Tree[Either[A, B]]], closed: List[Option[Tree[B]]]): List[Tree[B]] = {
        println(s"open $open\nclosed $closed\n")
        open match {
          case Branch(l, r) :: tail       => loop(l :: r :: tail, None :: closed)
          case Leaf(Left(value)) :: tail  => loop(func(value) :: tail, closed)
          case Leaf(Right(value)) :: tail => loop(tail, Some(pure(value)) :: closed)
          case Nil =>
            closed.foldLeft(Nil: List[Tree[B]]) { (acc: List[Tree[B]], maybeTree: Option[Tree[B]]) =>
              {
                val option: Option[List[Tree[B]]] = maybeTree.map((value: Tree[B]) => value :: acc)
                option.getOrElse {
                  val left :: right :: tail = acc
                  Branch(left, right) :: tail
                }
              }
            }
        }
      }
      loop(List(func(arg)), Nil).head
    }
  }

//  val res5: Tree[Int] = Branch(
//    Branch(Leaf(99), Leaf(101)),
//    Branch(Leaf(199), Leaf(201))
//  )

  val res5: Tree[Int] = Branch(Leaf(199), Leaf(201))

  println(treeMonad.tailRecM[Int, String](1)((arg) => {
    treeMonad.flatMap(res5)(a => Leaf(s"node $a".asRight[Int]))
  }))
}
// B1(L1, L2) -> B1(B2(L11, L12), B3(L21, L22))
