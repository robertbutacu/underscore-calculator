package data

import scala.util.Try

sealed trait Calculation[A] {
  def eval()(implicit n: Fractional[A]): Either[String, A] = {
    def applyOp(a: A, b: A, op: (A, A) => A) = op(a, b)

    this match {
      case Add(a, b) => for {
        r1 <- a.eval()
        r2 <- b.eval()
      } yield applyOp(r1, r2, n.plus)
      case Subtract(a, b) => for {
        r1 <- a.eval()
        r2 <- b.eval()
      } yield applyOp(r1, r2, n.minus)
      case Multiply(a, b) => for {
        r1 <- a.eval()
        r2 <- b.eval()
      } yield applyOp(r1, r2, n.times)
      case Divide(a, b) =>
        val bEvaluated = b.eval()

        if(bEvaluated.exists(v => n.equiv(n.zero, v)))
          Left("Division by zero!")
        else for {
          r1 <- a.eval()
          r2 <- bEvaluated
        } yield applyOp(r1, r2, n.div)
      case Self(v) => Right(v)
    }
  }

  def prettyPrinter(): String = {
    val opening: List[String] = List("(", "[", "{")
    val closing: List[String] = List(")", "]", "}")

    def getDepthLevelBracesOpening(level: Int): String = Try {
      opening(level)
    }.getOrElse(opening.last)

    def getDepthLevelBracesClosing(level: Int): String = Try {
      closing(level)
    }.getOrElse(closing.last)

    def getDepth(curr: Calculation[A]): Int = {
      curr match {
        case Add(a, b) => 1 + Math.max(getDepth(a), getDepth(b))
        case Subtract(a, b) => 1 + Math.max(getDepth(a), getDepth(b))
        case Multiply(a, b) => Math.max(getDepth(a), getDepth(b))
        case Divide(a, b) => Math.max(getDepth(a), getDepth(b))
        case Self(v) => 0
      }
    }

    val maxDepth = getDepth(this) - 1

    def go(curr: Calculation[A], depthLevel: Int = 1): String = {
      val opening = getDepthLevelBracesOpening(Math.min(2, maxDepth - depthLevel))
      val closing = getDepthLevelBracesClosing(Math.min(2, maxDepth - depthLevel))

      curr match {
        case Add(s: Self[A], b: Self[A]) => s"""${go(s)} + ${go(b)}"""
        case Add(s: Self[A], b) => s"""${go(s)} + $opening ${go(b, depthLevel + 1)} $closing """
        case Add(s, b: Self[A]) => s""" $opening ${go(s, depthLevel + 1)} $closing + ${b.prettyPrinter()}"""
        case Add(a, b) => s""" $opening ${go(a, depthLevel + 1)} $closing + $opening ${go(b, depthLevel + 1)} $closing """
        case Subtract(s: Self[A], b: Self[A]) => s"""${go(s)} - ${go(b)}"""
        case Subtract(s: Self[A], b) => s"""${go(s)} - $opening ${go(b, depthLevel + 1)} $closing """
        case Subtract(s, b: Self[A]) => s""" $opening ${go(s, depthLevel + 1)} $closing - ${b.prettyPrinter()}"""
        case Subtract(a, b) => s"""$opening ${go(a, depthLevel + 1)} $closing - $opening ${go(b, depthLevel + 1)} $closing """"
        case Multiply(a, b) => s"""${go(a, depthLevel + 1)} * ${go(b, depthLevel + 1)}"""
        case Divide(a, b) => s"""${go(a, depthLevel + 1)} / ${go(b, depthLevel + 1)}"""
        case Self(v) => v.toString
      }
    }

    go(this)
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