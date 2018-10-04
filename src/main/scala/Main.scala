import data.{Add, Multiply}

object Main extends App {
  println(Add(Multiply(1.0, 2.0), Multiply(3.0, 4.0)).prettyPrinter())
  println(Add(3.0, 4.0).prettyPrinter())
}
