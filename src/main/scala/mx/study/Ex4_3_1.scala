package mx.study

import cats.Monad
import cats.instances.option._
import cats.syntax.applicative._
import cats.syntax.flatMap._
import cats.syntax.functor._

import scala.annotation.tailrec
import scala.language.higherKinds


object Ex4_3_1 extends App {

  type Id[A] = A

  val idMonad = new Monad[Id] {
    self =>
    override def flatMap[A, B](a: A)(fn: A => Id[B]): Id[B] = fn(a)

    override def pure[A](a: A): Id[A] = a

    @tailrec
    override def tailRecM[A, B](a: A)(f: A => Id[Either[A, B]]): Id[B] = f(a) match {
      case Left(x) => self.tailRecM(x)(f)
      case Right(y) => y
    }
  }

  println(3.pure[Id])

  def add[F[_] : Monad](a: F[Int], b: F[Int]): F[Int] = for {
    x <- a
    y <- b
  } yield x + y

  println(add(Option(3), Option(4)))
  println(add(3: Id[Int], 0: Id[Int]))
}
