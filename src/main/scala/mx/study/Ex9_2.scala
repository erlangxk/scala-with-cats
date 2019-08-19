package mx.study

import cats.Foldable
import cats.kernel.Monoid

import scala.language.higherKinds

object Ex9_2 {

  def foldMap[A, B: Monoid](es: Vector[A])(f: A => B) = {
    //es.foldLeft(Monoid[B].empty)((acc,e)=> Monoid[B].combine(acc, f(e)))
    es.map(f).reduce(Monoid[B].combine)
  }

  def foldMap2[F[_] : Foldable, A, B: Monoid](es: F[A])(f: A => B): B = {
    Foldable[F].foldLeft(es, Monoid[B].empty)((acc, e) => Monoid.combine(acc, f(e)))
  }


}
