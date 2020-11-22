package edu.luc.cs.laufer.cs371.expressions
import scala.collection.mutable.{ Map => MMap }

object CombinatorCalculator extends App {

  def processExpr(input: String): Unit = {
    println("You entered: " + input)
    val result = CombinatorParser.parseAll(CombinatorParser.top_level, input)
    if (result.isEmpty) {
      println("This expression could not be parsed")
    } else {
      import behaviors._
      val expr = result.get
      println("Original expr:")
      println(expr)
      println("The parsed expression is: ")
      println(toFormattedString(expr))
      println("It has size " + size(expr) + " and height " + height(expr))
      println("It evaluates to " + evaluate(expr))
      println("The unparsed expression is: ")
      println(toUnparsed(expr))

      // Doing the execute stuff
      import Execute._
      println("Executing the infix expression...")
      println(apply(MMap[String, Int]())(expr))
    }
  }

  if (args.length > 0) {
    processExpr(args mkString " ")
  } else {
    print("Enter infix expression: ")
    scala.io.Source.stdin.getLines() foreach { line =>
      processExpr(line)
      print("Enter infix expression: ")
    }
  }
}
