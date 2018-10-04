package data

sealed trait Calculation[A] {
  def eval()(implicit n: Fractional[A]): A = {
    this match {
      case Add(a, b)      => n.plus(a.eval(), b.eval())
      case Subtract(a, b) => n.minus(a.eval(), b.eval())
      case Multiply(a, b) => n.times(a.eval(), b.eval())
      case Divide(a, b)   => n.div(a.eval(), b.eval())
      case Self(v)        => v
    }
  }

  def prettyPrinter(): String = {
    this match {
      case Add(s: Self[A], b: Self[A]) => s"""${s.prettyPrinter()} + ${b.prettyPrinter()}"""
      case Add(s: Self[A], b) => s"""${s.prettyPrinter()} + ( ${b.prettyPrinter()} ) """
      case Add(s, b: Self[A]) => s""" ( ${s.prettyPrinter()} ) + ${b.prettyPrinter()}"""
      case Add(a, b) => s""" ( ${a.prettyPrinter()} ) + ( ${b.prettyPrinter()}) """
      case Subtract(a, b) => s"""( ${a.prettyPrinter()} ) - ( ${b.prettyPrinter()} ) """"
      case Multiply(a, b) => s"""${a.prettyPrinter()} * ${b.prettyPrinter()}"""
      case Divide(a, b) => s"""${a.prettyPrinter()} / ${b.prettyPrinter()}"""
      case Self(v)      => v.toString
    }
  }
}

object Calculation {
  implicit def toSelf(d: Double): Self[Double] = Self(d)
}

case class Add[A](a: Calculation[A], b: Calculation[A]) extends Calculation[A]
case class Subtract[A](a: Calculation[A], b: Calculation[A]) extends Calculation[A]
case class Multiply[A](a: Calculation[A], b: Calculation[A]) extends Calculation[A]
case class Divide[A](a: Calculation[A], b: Calculation[A]) extends Calculation[A]

case class Self[A](value: A) extends Calculation[A]