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

  def readOperand(v: Int): Calculator[Unit] = State.modify(s => v :: s)

  private def popOne: Calculator[Int] = State[List[Int], Int] { s =>
    (s.tail, s.head)
  }

  def readOperator(op: Operator): Calculator[Int] = for {
    a <- popOne
    b <- popOne
    r = op.run(a, b)
    _ <- State.modify[List[Int]](r :: _)
  } yield r

  def read(s: Char) = if (s.isDigit) readOperand(s.toInt) else readOperator(Operator.read(s))
  
  val fs = for {
    _ <- readOperand(1)
    _ <- readOperand(2)
    r <- readOperator(Operator.Add)
    _ <- readOperand(3)
    r2 <- readOperator(Operator.Multiply)
  } yield (r, r2)

  val r = fs.run(List())
  println(r.value)

}
