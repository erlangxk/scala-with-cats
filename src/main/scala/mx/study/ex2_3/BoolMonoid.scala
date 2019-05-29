package mx.study.ex2_3

trait Semigroup[A] {
  def combine(a: A, b: A): A
}

trait Monoid[A] extends Semigroup[A] {
  def empty: A
}

object Monoid {
  def apply[A](implicit ev: Monoid[A]) = ev
}

object BoolMonoidInstance {

  implicit val boolAndInstance = new Monoid[Boolean] {
    override def combine(a: Boolean, b: Boolean): Boolean = a && b

    override def empty: Boolean = true
  }

  implicit val boolOrInstance = new Monoid[Boolean] {
    override def combine(a: Boolean, b: Boolean): Boolean = a || b

    override def empty: Boolean = false
  }

}


object BoolMonoidSytax {

  implicit class BoolMonoidOps[A](a: A) {
    def add(b: A)(implicit ev: Monoid[A]): A = ev.combine(a, b)
  }

}

object BoolMonoid extends App {
  def run(se: => Unit): Unit = se

  val a: Boolean = true
  val b: Boolean = false

  run {
    import BoolMonoidInstance.boolAndInstance
    println(Monoid[Boolean].combine(a, b))
    import BoolMonoidSytax._
    println(a.add(b))
  }

  run {
    import BoolMonoidInstance.boolOrInstance
    println(Monoid[Boolean].combine(a, b))
  }

}
