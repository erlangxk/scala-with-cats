package mx.study

import cats.Monoid

import  scala.language.higherKinds

object GCounterSimple {

  final case class GCounter(counters: Map[String, Int]) {
    def increment(machine: String, amount: Int) = {
      val total = counters.getOrElse(machine, 0)
      GCounter(counters.updated(machine, total + amount))
    }

    def merge(that: GCounter): GCounter = {
      val result = that.counters ++ this.counters.map { case (k, v) =>
        k -> scala.math.max(that.counters.getOrElse(k, 0), v)
      }
      GCounter(result)
    }

    def total: Int = counters.values.sum
  }

}

trait BoundedSemiLattice[A] extends Monoid[A] {
  def combine(a1: A, a2: A): A

  def empty: A
}

object BoundedSemiLattice {
  def apply[A: BoundedSemiLattice] = implicitly[BoundedSemiLattice[A]]

  implicit val intBoundedSemiLattice = new BoundedSemiLattice[Int] {
    def combine(a1: Int, a2: Int) = scala.math.max(a1, a2)

    def empty: Int = 0
  }

  implicit def setBoundedSemiLattice[A] = new BoundedSemiLattice[Set[A]] {
    def combine(a1: Set[A], a2: Set[A]) = a1 union a2

    def empty: Set[A] = Set.empty[A]
  }

}

object GCounterMonoid {

  final case class GCounter[A](counters: Map[String, A]) {
    def increment(machine: String, amount: A)(implicit ev: Monoid[A]) = {
      val total = counters.getOrElse(machine, ev.empty)
      GCounter(counters.updated(machine, ev.combine(total, amount)))
    }

    def merge(that: GCounter[A])(implicit ev: BoundedSemiLattice[A]): GCounter[A] = {
      val result = that.counters ++ this.counters.map {
        case (k, v) => {
          val nr = that.counters.getOrElse(k, BoundedSemiLattice[A].empty)
          k -> BoundedSemiLattice[A].combine(nr, v)
        }
      }
      GCounter(result)
    }

    def total(implicit ev: Monoid[A]): A = counters.values.foldLeft(ev.empty)(ev.combine)
  }

}

trait KVStore[F[_, _], K, V] {
  def increment(f: F[K, V])(k: K, v: V)(implicit m: Monoid[V]): F[K, V]

  def merge(f1: F[K, V], f2: F[K, V])(implicit b: BoundedSemiLattice[V]): F[K, V]

  def total(f: F[K, V])(implicit m: Monoid[V]): V
}

object KVStore {
  implicit def mapKVStore[K, V] = new KVStore[Map, K, V] {
    override def increment(f: Map[K, V])(k: K, v: V)(implicit m: Monoid[V]): Map[K, V] = {
      val t = f.getOrElse(k, m.empty)
      f.updated(k, m.combine(t, v))
    }

    override def merge(f1: Map[K, V], f2: Map[K, V])(implicit b: BoundedSemiLattice[V]) = {
      f1 ++ f2.map { case (k, v) =>
        k -> b.combine(f1.getOrElse(k, b.empty), v)
      }
    }

    override def total(f: Map[K, V])(implicit m: Monoid[V]): V =
      f.foldLeft(m.empty)((acc, e) => m.combine(e._2, acc))
  }
}


object Ex11_2_3 extends App {

  import cats.instances.int._

  val g1 = Map("a" -> 7, "b" -> 3, "c" -> 1)
  val g2 = Map("a" -> 2, "b" -> 4, "d" -> 2)

  val counter = implicitly[KVStore[Map, String, Int]]

  val merged = counter.merge(g1, g2)

  val total = counter.total(merged)

  println(merged)
  println(total)


  // a merge b merge c  = (a merge b) merge c = a merge (b merge c) //associativity

  // a merge b = b merge a // commutativity
}





