package mx.study

object Ex3_6_1_1 extends App {

  trait Printable[A] {
    self =>
    def format(value: A): String

    def contramap[B](f: B => A): Printable[B] = new Printable[B] {
      override def format(value: B): String = self.format(f(value))
    }
  }


  implicit val stringPrintable: Printable[String] = new Printable[String] {
    override def format(value: String): String = s""""$value""""
  }

  //implicit val booleanPrintable: Printable[Boolean] = (b: Boolean) => if (b) "yes" else "no"
  //implicit val booleanPrintable = stringPrintable.contramap((b:Boolean) => if (b) "yes" else "no")
  implicit val booleanPrintable = stringPrintable.contramap[Boolean](b => if (b) "yes" else "no")

  def format[A: Printable](a: A) = implicitly[Printable[A]].format(a)

  println(format("hello"))

  println(format(true))

  final case class Box[A](value: A)

  implicit def boxPrintable[A](implicit ev: Printable[A]): Printable[Box[A]] = ev.contramap(b => b.value)

  println(format(Box("wokao")))
  println(format(Box(true)))
}
