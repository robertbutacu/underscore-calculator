import data.{Add, Divide, Multiply, Subtract}

object Main extends App {
  println(Add(Add(Multiply(1.0, 2.0), Subtract(3.0, 4.0)), Divide(3.0, 1.0)).eval())
  println(Add(3.0, 4.0).prettyPrinter())
}
