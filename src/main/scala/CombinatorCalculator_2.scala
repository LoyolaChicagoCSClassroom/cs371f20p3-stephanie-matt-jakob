package edu.luc.cs.laufer.cs371.expressions
import scala.collection.mutable.{ Map => MMap }
import Execute_num._

object CombinatorCalculator_2 extends App {

  def processExpr(input: String, store: MMap[String, Num]): Unit = {
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
      println("Executing the infix expression...")
      println(apply(store)(expr))
      println("Memory: " + store.toString())
    }
  }

  // Prepating an empty map for the store function
  var store = MMap[String, Num]()

  if (args.length > 0) {
    processExpr(args mkString " ", MMap[String, Num]())
  } else {
    print("Enter infix expression: ")
    scala.io.Source.stdin.getLines() foreach { line =>
      processExpr(line, store)
      print("Enter infix expression: ")
    }
  }
}
