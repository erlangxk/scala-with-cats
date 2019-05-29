package mx.study.ex1_3

trait Printable[A] {
  def format(a: A): String
}


object Printable {
  def format[A](a: A)(implicit ev: Printable[A]) = ev.format(a)
  def print[A: Printable](a: A): Unit = println(format(a))
}

object PrintableSyntax {

  implicit class PrintableOps[A](a: A) {
    def format(implicit ev: Printable[A]): String = Printable.format(a)
    def print(implicit ev: Printable[A]): Unit = Printable.print(a)
  }

}

object PrintableInstances {
  implicit val stringPrintable = new Printable[String] {
    override def format(a: String): String = a
  }

  implicit val intPrintable = new Printable[Int] {
    override def format(a: Int): String = a.toString
  }
}

final case class Cat(name: String, age: Int, color: String)

object Cat {

  import PrintableInstances._

  implicit val catPrintable = new Printable[Cat] {
    override def format(a: Cat): String = s"${Printable.format(a.name)} is a ${Printable.format(a.age)} year-old ${Printable.format(a.color)} cat"
  }
}

object Ex1_3App extends App {

  val cat = Cat("miao", 3, "brown")
  Printable.print(cat)

  import PrintableSyntax._

  cat.print
}
