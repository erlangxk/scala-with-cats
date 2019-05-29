package mx.study

import cats.Monoid
import cats.instances.int._
import cats.instances.option._
import cats.syntax.semigroup._

object Ex2_5_4 extends App {

  def addImpl[A: Monoid](items: List[A]) = items.foldLeft(Monoid[A].empty)(_ |+| _)

  def add(items: List[Int]): Int = addImpl(items)

  def add2(items: List[Option[Int]]): Option[Int] = addImpl(items)

  println(add(List(1, 2, 3)))
  println(add2(List(Some(1), Some(2), Some(3))))
  println(add2(List(Some(1), Some(2), None)))
  println(add2(List(None, None)))

  case class Order(totalCost: Double, quantity: Double)

  object Order {
    implicit val orderMonoid = new Monoid[Order] {
      override def combine(x: Order, y: Order): Order = Order(x.totalCost + y.totalCost, x.quantity + y.quantity)

      override def empty: Order = Order(0, 0)
    }
  }

  println(addImpl(List(Order(3, 7), Order(4, 8))))
}
