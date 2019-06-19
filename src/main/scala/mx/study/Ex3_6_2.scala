package mx.study

object Ex3_6_2 extends App {

  trait Codec[A] {
    self =>
    def encode(value: A): String

    def decode(s: String): A

    def imap[B](dec: A => B, enc: B => A): Codec[B] = new Codec[B] {
      override def decode(s: String): B = dec(self.decode(s))

      override def encode(value: B): String = self.encode(enc(value))
    }
  }

  object Codec {
    def encode[A](value: A)(implicit ev: Codec[A]) = ev.encode(value)

    def decode[A](s: String)(implicit ev: Codec[A]) = ev.decode(s)
  }

  case class Box[A](value: A)

  object CodecInstances {
    implicit val stringCodec: Codec[String] = new Codec[String] {
      override def decode(s: String): String = s

      override def encode(s: String): String = s
    }

    implicit val intCodec: Codec[Int] = stringCodec.imap(_.toInt, _.toString)

    implicit val doubleCodec: Codec[Double] = stringCodec.imap(_.toDouble, _.toString)

    implicit def boxCodec[A](implicit ev: Codec[A]): Codec[Box[A]] = ev.imap(a => Box(a), b => b.value)
  }

  import CodecInstances._

  println(Codec.encode(Box(34)))
  println(Codec.decode[Box[Int]]("34"))
}


