package mx.study

import cats.Eval

object Ex4_6_5  extends  App{

  def foldRight[A, B](as: List[A], acc: B)(fn: (A, B) => B): B = as match {
    case head :: tail => fn(head, foldRight(tail, acc)(fn))
    case Nil =>acc
  }

  def foldRight2[A, B](as: List[A], acc: B)(fn: (A, B) => B): Eval[B] = as match {
    case head :: tail =>  Eval.defer(foldRight2(tail, acc)(fn).map(v=>fn(head,v)))
    case Nil =>Eval.now(acc)
  }


  val z=foldRight2( (0 to 10000).toList, 0)(_ + _)
  println(z.value)
  
}
