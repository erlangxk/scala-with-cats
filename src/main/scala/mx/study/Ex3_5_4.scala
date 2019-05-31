package mx.study

import cats.Functor
import cats.syntax.functor._

object Ex3_5_4 extends App {

  sealed trait Tree[+A]

  final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  final case class Leaf[A](value: A) extends Tree[A]

  object Tree {
    implicit val functorTree = new Functor[Tree] {
      override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = fa match {
        case Leaf(v) => Leaf(f(v))
        case Branch(lt, rt) => Branch(map(lt)(f), map(rt)(f))
      }
    }
  }

  val t: Tree[Int] = Branch(Leaf(3), Branch(Leaf(4), Leaf(5)))
  val r = t.map(a => a + 3)
  println(r)

}
