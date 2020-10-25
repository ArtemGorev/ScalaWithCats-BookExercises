package cats.chapter3

import cats.Functor

object TreeFunctorApp extends App {

  sealed trait Tree[+A]
  final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
  final case class Leaf[A](value: A)                        extends Tree[A]

  val treeFunctor = new Functor[Tree] {
    def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = fa match {
      case Leaf(value)         => Leaf(f(value))
      case Branch(left, right) => Branch(map(left)(f), map(right)(f))
    }
  }

  val tree1: Tree[Int] = Branch(Leaf(1), Leaf(2))
  println(treeFunctor.map(tree1)(a => s"$a => 123"))
  val tree2: Tree[Int] = Branch(Leaf(1), Branch(Leaf(1), Branch(Leaf(1), Branch(Leaf(1), Leaf(2)))))
  println(treeFunctor.map(tree2)(_ * 10))

}
