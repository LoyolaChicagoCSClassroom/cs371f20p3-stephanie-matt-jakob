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
    case UMinus(r) => buildUnaryExprString(prefix, "UMinus", toFormattedString(prefix)(r))
    case Constant(c) => buildUnaryExprString(prefix, "Constant", c.toString)
    case Variable(v) => buildUnaryExprString(prefix, "Variable", v)
    case Plus(l, r) => buildExprString(prefix, "Plus", toFormattedString(prefix)(l), toFormattedString(prefix)(r))
    case Minus(l, r) => buildExprString(prefix, "Minus", toFormattedString(prefix)(l), toFormattedString(prefix)(r))
    case Times(l, r) => buildExprString(prefix, "Times", toFormattedString(prefix)(l), toFormattedString(prefix)(r))
    case Div(l, r) => buildExprString(prefix, "Div", toFormattedString(prefix)(l), toFormattedString(prefix)(r))
    case Mod(l, r) => buildExprString(prefix, "Mod", toFormattedString(prefix)(l), toFormattedString(prefix)(r))
    case Assign(l, r) => buildExprString(prefix, "Assign", toFormattedString(prefix)(l), toFormattedString(prefix)(r))
    case Field(k, v) => buildExprString(prefix, "Field", k, toFormattedString(prefix)(v))
    case Block(statements @ _*) => {
      // Block top level, pass in all formated exprs to build urnary string
      val block_list = statements.map(s => toFormattedString(prefix)(s))
      buildBlockExprString(prefix, "Block", block_list: _*)
      // buildExprString(prefix, "Assign", toFormattedString(prefix)(l), toFormattedString(prefix)(r))
    }
    case Struct(fields @ _*) => {
      val struct_list = fields.map(s => toFormattedString(prefix)(s))
      buildStructExprString(prefix, "Struct", struct_list: _*)
    }
    case Loop(x, y) => buildExprString(prefix, "Loop", toFormattedString(prefix)(x), toFormattedString(prefix)(y))
    case Cond(x, y, z) => buildExprString(prefix, "Cond", toFormattedString(prefix)(x), toFormattedString(prefix)(y) + toFormattedString(prefix)(z))
  }

  def toFormattedString(e: Expr): String = toFormattedString("")(e)

  def buildExprString(prefix: String, nodeString: String, leftString: String, rightString: String) = {
    val result = new StringBuilder(prefix)
    result.append(nodeString)
    result.append("(")
    result.append(leftString)
    result.append(",")
    result.append(rightString)
    result.append(")")
    result.toString
  }

  def buildBlockExprString(prefix: String, nodeString: String, exprStrings: String*) = {
    val result = new StringBuilder(prefix)
    result.append(nodeString)
    result.append("(")
    exprStrings.map(s => {
      result.append(s)
    })
    result.append(")")
    result.append(",")
    result.toString
  }

  def buildStructExprString(prefix: String, nodeString: String, exprStrings: String*) = {
    val result = new StringBuilder(prefix)
    result.append(nodeString)
    result.append("(")
    exprStrings.map(s => {
      result.append(s)
    })
    result.append(")")
    result.append(",")
    result.toString
  }

  def buildUnaryExprString(prefix: String, nodeString: String, exprString: String) = {
    val result = new StringBuilder(prefix)
    result.append(nodeString)
    result.append("(")
    result.append(exprString)
    result.append(")")
    result.toString
  }

  def toUnparsed(e: Expr): String = toUnparsed("")(e)

  def toUnparsed(prefix: String)(e: Expr): String = e match {
    case Constant(c) => buildUnaryExprUnparsed(prefix, c.toString)
    case Variable(v) => buildUnaryExprUnparsed(prefix, v)
    case UMinus(r) => buildUnaryExprUnparsed(prefix, toUnparsed(prefix)(r))
    case Plus(l, r) => buildExprUnparsed(prefix, " + ", toUnparsed(prefix)(l), toUnparsed(prefix)(r))
    case Minus(l, r) => buildExprUnparsed(prefix, " - ", toUnparsed(prefix)(l), toUnparsed(prefix)(r))
    case Times(l, r) => buildExprUnparsed(prefix, " * ", toUnparsed(prefix)(l), toUnparsed(prefix)(r))
    case Div(l, r) => buildExprUnparsed(prefix, " / ", toUnparsed(prefix)(l), toUnparsed(prefix)(r))
    case Mod(l, r) => buildExprUnparsed(prefix, " % ", toUnparsed(prefix)(l), toUnparsed(prefix)(r))
    case Assign(l, r) => buildAssignExprUnparsed(prefix, " = ", toUnparsed(prefix)(l), toUnparsed(prefix)(r))
    case Field(k, v) => buildFieldExprUnparsed(prefix, ": ", k, toUnparsed(prefix)(v))
    case Block(statements @ _*) => {
      // Block top level, pass in all formated exprs to build urnary string
      val block_list = statements.map(s => toUnparsed(prefix + INDENT)(s)) //add indent after prefix
      buildBlockExprUnparsed(prefix, block_list: _*)
    }
    case Struct(fields @ _*) => {
      val field_list = fields.map(s => toUnparsed(prefix)(s))
      buildStructExprUnparsed(prefix, field_list: _*)
    }
    case Loop(x, y) => buildLoopExprUnparsed(prefix, "while (", toUnparsed(prefix)(x), toUnparsed(prefix)(y))
    case Cond(x, y, z) => buildCondExprUnparsed(prefix, "if (", toUnparsed(prefix)(x), toUnparsed(prefix)(y) + " else" + toUnparsed(prefix)(z))
  }

  def buildCondExprUnparsed(prefix: String, nodeString: String, leftString: String, rightString: String) = {
    val result = new StringBuilder(prefix)
    result.append(nodeString)
    result.append(leftString)
    result.append(")")
    result.append(rightString)
    result.toString
  }

  def buildLoopExprUnparsed(prefix: String, nodeString: String, leftString: String, rightString: String) = {
    val result = new StringBuilder(prefix)
    result.append(nodeString)
    result.append(leftString)
    result.append(")")
    result.append(rightString)
    result.toString
  }

  def buildAssignExprUnparsed(prefix: String, nodeString: String, leftString: String, rightString: String) = {
    val result = new StringBuilder(prefix)
    result.append(leftString)
    result.append(nodeString)
    result.append(rightString)
    result.append(";")
    result.toString
  }

  def buildFieldExprUnparsed(prefix: String, nodeString: String, leftString: String, rightString: String) = {
    val result = new StringBuilder()
    result.append(leftString)
    result.append(nodeString)
    result.append(rightString)
    result.toString
  }

  def buildExprUnparsed(prefix: String, nodeString: String, leftString: String, rightString: String) = {
    val result = new StringBuilder()
    result.append("(")
    result.append(leftString)
    result.append(nodeString)
    result.append(rightString)
    result.append(")")
    result.toString
  }

  def buildBlockExprUnparsed(prefix: String, exprStrings: String*) = {
    val result = new StringBuilder(prefix)
    result.append("{")
    // result.append(EOL)
    exprStrings.map(s => {
      result.append(EOL)
      // result.append(prefix + INDENT)
      result.append(s)
    })
    result.append(EOL)
    result.append(prefix)
    result.append("}")
    result.toString
  }

  def buildStructExprUnparsed(prefix: String, exprStrings: String*) = {
    val result = new StringBuilder()
    result.append("{ ")
    val iter = Iterator(exprStrings)
    exprStrings.map(s => {
      result.append(s)
      if(iter.hasNext) { 
        result.append(", ")
        iter.next()
      }
    })
    result.append(" }")
    result.toString
  }

  def buildUnaryExprUnparsed(prefix: String, exprString: String) = {
    val result = new StringBuilder()
    result.append(exprString)
    result.toString
  }

  val EOL = scala.util.Properties.lineSeparator
  val INDENT = "  "
}
