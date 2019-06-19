package mx.study

import cats.Monad

object Ex4_10_1 extends App {

  sealed trait Tree[+A]

  final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  final case class Leaf[A](value: A) extends Tree[A]

  object Tree {
    def branch[A](left: Tree[A], right: Tree[A]): Tree[A] = Branch(left, right)
    def leaf[A](value: A): Tree[A] = Leaf(value)
  }

  implicit val treeMonadInstance = new Monad[Tree] {
    self =>
    override def pure[A](value: A): Tree[A] = Tree.leaf(value)

    override def flatMap[A, B](t: Tree[A])(f: A => Tree[B]): Tree[B] = t match {
      case Leaf(v) => f(v)
      case Branch(left, right) => Tree.branch(self.flatMap(left)(f), self.flatMap(right)(f))
    }

    override def tailRecM[A, B](a: A)(f: A => Tree[Either[A, B]]): Tree[B] = {
      def handleTree(t: Tree[Either[A, B]]): Tree[B] = t match {
        case Leaf(Left(x)) => tailRecM(x)(f)
        case Leaf(Right(y)) => Tree.leaf(y)
        case Branch(l, r) => Branch(handleTree(l), handleTree(r))
      }
      handleTree(f(a))
    }
  }

  val t = Tree.branch(Tree.leaf(4), Tree.leaf(9))

  def f(t: Int): Tree[Int] = Tree.branch(Tree.leaf(t - 1), Tree.leaf(t + 1))


  val r = Monad[Tree].flatMap(t)(f)

  println(r)


}
