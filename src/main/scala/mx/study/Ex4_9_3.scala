package mx.study

import cats.data.State

sealed trait Operator {
  def run(a: Int, b: Int): Int
}

object Operator {
  case object Add extends Operator {
    def run(a: Int, b: Int) = a + b
  }

  case object Multiply extends Operator {
    def run(a: Int, b: Int) = a * b
  }

  case object Subtract extends Operator {
    def run(a: Int, b: Int) = a - b
  }

  case object Divide extends Operator {
    def run(a: Int, b: Int) = a / b
  }

  def read(c: Char): Operator = c match {
    case '/' => Divide
    case '-' => Subtract
    case '*' => Multiply
    case '+' => Add
    case _ => throw new IllegalArgumentException
  }
}

object Ex4_9_3 extends App {

  type Calculator[A] = State[List[Int], A]

  def readOperand(v: Int): Calculator[Int] = State { s =>
    (v::s, v)
  }

  private def popOne: Calculator[Int] = State[List[Int], Int] { s =>
    (s.tail, s.head)
  }

  def readOperator(op: Operator): Calculator[Int] = for {
    a <- popOne
    b <- popOne
    r = op.run(a, b)
    _ <- State.modify[List[Int]](r :: _)
  } yield r

  val fs = for {
    _ <- readOperand(1)
    _ <- readOperand(2)
    r <- readOperator(Operator.Add)
    _ <- readOperand(3)
    r2 <- readOperator(Operator.Multiply)
  } yield (r, r2)

  val r = fs.run(List())
  println(r.value)

  def readChar(s: Char) = if (s.isDigit) readOperand(Character.getNumericValue(s)) else readOperator(Operator.read(s))

  def read(s:String):Calculator[Int] = s.toCharArray.map(readChar).toList match {
    case Nil => throw new IllegalArgumentException
    case h :: Nil => h
    case l => l.reduce((s1, s2) => s1.flatMap(_ => s2))
  }

  println(read("12+3*").run(Nil).value)

  val program = for {
    _ <- read("12+")
    _ <- read("34+")
    ans <- readChar('*')
  } yield ans

  println(program.run(Nil).value)

}
