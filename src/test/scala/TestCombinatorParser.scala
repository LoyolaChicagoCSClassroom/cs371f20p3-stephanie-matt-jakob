package edu.luc.cs.laufer.cs371.expressions

import org.scalatest.funsuite.AnyFunSuite
import scala.collection.mutable.{ Map => MMap }
import Execute_num_value._
import behaviors._
import TestFixtures._
import scala.collection.mutable.{ Map => MMap }
import Execute_num_value._

object MainCombinatorParser extends App {
  val parsedExpr = CombinatorParser.parseAll(CombinatorParser.expr, complex1string)
  println(parsedExpr.get)
  println(complex1)
  println(parsedExpr.get == complex1)
  println(behaviors.evaluate(parsedExpr.get))
}

class TestCombinatorParser extends AnyFunSuite {
  // assignment test
  var storeAssign = MMap[String, Value]()
  val parsedExpr = CombinatorParser.parseAll(CombinatorParser.statement, assignmentString)
  test("assignment parser test") { assert(parsedExpr.get === assignment) }
  test("assignment unparser test") { assert(toUnparsed(parsedExpr.get) === assignmentUnpars) }
  val parsedExprMap = CombinatorParser.parseAll(CombinatorParser.top_level, assignmentMapString)
  apply(storeAssign)(parsedExprMap.get)
  test("assignment map test"){ assert(storeAssign.toString === assignmentMap)}

  // loop test
  var storeLoop = MMap[String, Value]()
  val parsedExpr2 = CombinatorParser.parseAll(CombinatorParser.statement, whileString)
  test("loop parser test") { assert(parsedExpr2.get === whileAST) }
  test("loop unparser test") { assert(toUnparsed(parsedExpr2.get) === whileUnpars) }
  val parsedExpr2map = CombinatorParser.parseAll(CombinatorParser.top_level, whileMapString)
  apply(storeLoop)(parsedExpr2map.get)
  test("loop map test") { assert(storeLoop.toString() === whileMap )}

  // condition test
  var storeCond = MMap[String, Value]()
  val parsedExpr3 = CombinatorParser.parseAll(CombinatorParser.statement, conditionString)
  test("condition parser test") { assert(parsedExpr3.get === conditionAST) }
  test("condition unparser test") { assert(toUnparsed(parsedExpr3.get) === conditionUnpars) }
  val parsedExpr3map = CombinatorParser.parseAll(CombinatorParser.top_level, condMapString)
  apply(storeCond)(parsedExpr3map.get)
  test("condition map test") { assert(storeCond.toString() === condMap )}

  // block test
  var storeBlock = MMap[String, Value]()
  val parsedExpr4 = CombinatorParser.parseAll(CombinatorParser.statement, blockString)
  test("block parser test") { assert(parsedExpr4.get === blockAST) }
  test("block unparser test") { assert(toUnparsed(parsedExpr4.get) === blockUnpars) }
  val parsedExpr4Map = CombinatorParser.parseAll(CombinatorParser.top_level, blockMapString)
  apply(storeBlock)(parsedExpr4Map.get)
  test("block map test"){ assert(storeBlock.toString === blockMap)}

  // complex test
  val parsedExpr5 = CombinatorParser.parseAll(CombinatorParser.expr, complex1string)
  val parsedExpr6 = CombinatorParser.parseAll(CombinatorParser.statement, complex1string2)
  test("complex 1 parser test") { assert(parsedExpr5.get === complex1) }
  test("complex 2 parser test") { assert(parsedExpr6.get === complex2) }

  // Error on unassigned var test
  var storeError = MMap[String, Value]()
  val parsedExpr7 = CombinatorParser.parseAll(CombinatorParser.top_level, throwErrorString).get
  test("error on unassigned var test"){ assert(apply(storeError)(parsedExpr7).toString === throwErrorOutput)}
}
