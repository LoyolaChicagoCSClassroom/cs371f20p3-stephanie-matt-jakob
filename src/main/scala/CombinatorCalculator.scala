package edu.luc.cs.laufer.cs371.expressions
import scala.collection.mutable.{ Map => MMap }
import Execute_num._

object CombinatorCalculator extends App {

  def processExpr(input: String, store: MMap[String, Value]): Unit = {
    println("You entered: " + input)
    val result = CombinatorParser.parseAll(CombinatorParser.top_level, input)
    if (result.isEmpty) {
      println("This expression could not be parsed")
    } else {
      import behaviors._
      val expr = result.get
      println("The parsed expression is: ")
      println(toFormattedString(expr))
      println("It has size " + size(expr) + " and height " + height(expr))
      println("The unparsed expression is: ")
      println(toUnparsed(expr))

      // Doing the execute stuff
      println("Executing the infix expression...")
      println("It evaluates to " + apply(store)(expr))
      println("Memory: " + store.toString()  + "\n")
    }
  }

  // Prepating an empty map for the store function
  var store = MMap[String, Value]()

  if (args.length > 0) {
    processExpr(args mkString " ", MMap[String, Value]())
  } else {
    println()
    print("Memory: " + store.toString()  + "\n")
    print("Enter infix expression: ")
    scala.io.Source.stdin.getLines() foreach { line =>
      processExpr(line, store)
      println()
      print("Memory: " + store.toString()  + "\n")
      print("Enter infix expression: ")
    }
  }
}
