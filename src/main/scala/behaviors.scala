package edu.luc.cs.laufer.cs371.expressions
import scala.collection.mutable.ListBuffer

import ast._

object behaviors {

  def evaluate(e: Expr): Int = e match {
    case Constant(c) => c
    case Variable(x) => 0
    case UMinus(r) => -evaluate(r)
    case Plus(l, r) => evaluate(l) + evaluate(r)
    case Minus(l, r) => evaluate(l) - evaluate(r)
    case Times(l, r) => evaluate(l) * evaluate(r)
    case Div(l, r) => evaluate(l) / evaluate(r)
    case Mod(l, r) => evaluate(l) % evaluate(r)
    case Assign(l, r) => evaluate(r)
    case Block(statements @ _*) => { val evaluate_list = statements.map(x => evaluate(x)); evaluate_list.sum }
    case Loop(x, y) => evaluate(y)
    case Cond(x, y, z) => evaluate(y) + evaluate(z)
  }

  def size(e: Expr): Int = e match {
    case Constant(c) => 1
    case Variable(v) => 1
    case UMinus(r) => 1 + size(r)
    case Plus(l, r) => 1 + size(l) + size(r)
    case Minus(l, r) => 1 + size(l) + size(r)
    case Times(l, r) => 1 + size(l) + size(r)
    case Div(l, r) => 1 + size(l) + size(r)
    case Mod(l, r) => 1 + size(l) + size(r)
    case Assign(l, r) => 1 + size(l) + size(r)
    case Block(statements @ _*) => { val count_list = statements.map(x => size(x)); count_list.sum }
    case Loop(x, y) => 1 + size(x) + size(y)
    case Cond(x, y, z) => 1 + size(x) + size(y) + size(z)
  }

  def height(e: Expr): Int = e match {
    case Constant(c) => 1
    case Variable(v) => 1
    case UMinus(r) => 1 + height(r)
    case Plus(l, r) => 1 + math.max(height(l), height(r))
    case Minus(l, r) => 1 + math.max(height(l), height(r))
    case Times(l, r) => 1 + math.max(height(l), height(r))
    case Div(l, r) => 1 + math.max(height(l), height(r))
    case Mod(l, r) => 1 + math.max(height(l), height(r))
    case Assign(l, r) => 1 + math.max(height(l), height(r))
    case Block(statements @ _*) => { 1 + statements.foldLeft(0) { (a, b) => Math.max(a, height(b)) } }
    case Loop(x, y) => 1 + math.max(height(x), height(y))
    case Cond(x, y, z) => 1 + math.max(math.max(height(x), height(y)), height(z))
  }

  def toFormattedString(prefix: String)(e: Expr): String = e match {
    case Constant(c) => buildUnaryExprString(prefix, "Constant", c.toString)
    case Variable(v) => buildUnaryExprString(prefix, "Variable", v)
    case UMinus(r) => buildUnaryExprString(prefix, "UMinus", toFormattedString(prefix + INDENT)(r))
    case Plus(l, r) => buildExprString(prefix, "Plus", toFormattedString(prefix + INDENT)(l), toFormattedString(prefix + INDENT)(r))
    case Minus(l, r) => buildExprString(prefix, "Minus", toFormattedString(prefix + INDENT)(l), toFormattedString(prefix + INDENT)(r))
    case Times(l, r) => buildExprString(prefix, "Times", toFormattedString(prefix + INDENT)(l), toFormattedString(prefix + INDENT)(r))
    case Div(l, r) => buildExprString(prefix, "Div", toFormattedString(prefix + INDENT)(l), toFormattedString(prefix + INDENT)(r))
    case Mod(l, r) => buildExprString(prefix, "Mod", toFormattedString(prefix + INDENT)(l), toFormattedString(prefix + INDENT)(r))
    case Assign(l, r) => buildExprString(prefix, "Assign", toFormattedString(prefix + INDENT)(l), toFormattedString(prefix + INDENT)(r))
    case Block(statements @ _*) => {
      // Block top level, pass in all formated exprs to build urnary string
      val block_list = statements.map(s => toFormattedString(prefix + INDENT)(s))
      buildBlockExprString(prefix, "Block", block_list: _*)
      // buildExprString(prefix, "Assign", toFormattedString(prefix + INDENT)(l), toFormattedString(prefix + INDENT)(r))
    }
    case Loop(x, y) => buildExprString(prefix, "Loop", toFormattedString(prefix + INDENT)(x), toFormattedString(prefix + INDENT)(y))
    case Cond(x, y, z) => buildExprString(prefix, "Cond", toFormattedString(prefix + INDENT)(x), toFormattedString(prefix + INDENT)(y) + toFormattedString(prefix + INDENT)(z))
  }

  def toFormattedString(e: Expr): String = toFormattedString("")(e)

  def buildExprString(prefix: String, nodeString: String, leftString: String, rightString: String) = {
    val result = new StringBuilder(prefix)
    result.append(nodeString)
    result.append("(")
    result.append(EOL)
    result.append(leftString)
    result.append(", ")
    result.append(EOL)
    result.append(rightString)
    result.append(")")
    result.toString
  }

  def buildBlockExprString(prefix: String, nodeString: String, exprStrings: String*) = {
    val result = new StringBuilder(prefix)
    result.append(nodeString)
    result.append("(")
    exprStrings.map(s => {
      result.append(EOL)
      result.append(s)
      result.append(", ")
    })
    result.append(")")
    result.toString
  }

  def buildUnaryExprString(prefix: String, nodeString: String, exprString: String) = {
    val result = new StringBuilder(prefix)
    result.append(nodeString)
    result.append("(")
    result.append(EOL)
    result.append(exprString)
    result.append(")")
    result.toString
  }

  val EOL = scala.util.Properties.lineSeparator
  val INDENT = ".."
}
