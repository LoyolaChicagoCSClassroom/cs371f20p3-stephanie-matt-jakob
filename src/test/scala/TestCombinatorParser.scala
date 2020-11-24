package edu.luc.cs.laufer.cs371.expressions

import org.scalatest.funsuite.AnyFunSuite
import scala.collection.mutable.{ Map => MMap }
import Execute_num._
import behaviors._
import TestFixtures._
import scala.collection.mutable.{ Map => MMap }
import Execute_num._

object MainCombinatorParser extends App {
  val parsedExpr = CombinatorParser.parseAll(CombinatorParser.expr, complex1string)
  println(parsedExpr.get)
  println(complex1)
  println(parsedExpr.get == complex1)
  println(behaviors.evaluate(parsedExpr.get))
}

class TestCombinatorParser extends AnyFunSuite {
  // Prepating an empty map for the store function
  var store = MMap[String, Num]()

  // assignment test
  val parsedExpr = CombinatorParser.parseAll(CombinatorParser.statement, assignmentString)
  test("assignment parser test") { assert(parsedExpr.get === assignment) }
  test("assignment unparser test") { assert(toUnparsed(parsedExpr.get) === assignmentUnpars) }

  // loop test
  var storeLoop = MMap[String, Num]()
  val parsedExpr2 = CombinatorParser.parseAll(CombinatorParser.statement, whileString)
  test("loop parser test") { assert(parsedExpr2.get === whileAST) }
  test("loop unparser test") { assert(toUnparsed(parsedExpr2.get) === whileUnpars) }
  val parsedExpr2map = CombinatorParser.parseAll(CombinatorParser.top_level, whileMapString)
  apply(storeLoop)(parsedExpr2map.get)
  test("loop map test") { assert(storeLoop.toString() === whileMap )}

  // condition test
  var storeCond = MMap[String, Num]()
  val parsedExpr3 = CombinatorParser.parseAll(CombinatorParser.statement, conditionString)
  test("condition parser test") { assert(parsedExpr3.get === conditionAST) }
  test("condition unparser test") { assert(toUnparsed(parsedExpr3.get) === conditionUnpars) }
  val parsedExpr3map = CombinatorParser.parseAll(CombinatorParser.top_level, condMapString)
  apply(storeCond)(parsedExpr3map.get)
  test("condition map test") { assert(storeCond.toString() === condMap )}

  // block test
  val parsedExpr4 = CombinatorParser.parseAll(CombinatorParser.statement, blockString)
  test("block parser test") { assert(parsedExpr4.get === blockAST) }
  test("block unparser test") { assert(toUnparsed(parsedExpr4.get) === blockUnpars) }

  // complex test
  val parsedExpr5 = CombinatorParser.parseAll(CombinatorParser.expr, complex1string)
  val parsedExpr6 = CombinatorParser.parseAll(CombinatorParser.statement, complex1string2)
  test("complex 1 parser test") { assert(parsedExpr5.get === complex1) }
  test("complex 2 parser test") { assert(parsedExpr6.get === complex2) }

  //assignment map test
  val parsedExpr7 = CombinatorParser.parseAll(CombinatorParser.statement, assignmentMapString)
  apply(store)(parsedExpr7.get)
  test("assignment map test"){ assert(store.toString === assignmentMap)}

  //block map test
  val parsedExpr8 = CombinatorParser.parseAll(CombinatorParser.statement, blockMapString)
  apply(store)(parsedExpr8.get)
  test("block map test"){ assert(store.toString === blockMap)}

}
