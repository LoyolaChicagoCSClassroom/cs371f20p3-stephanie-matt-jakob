package edu.luc.cs.laufer.cs371.expressions.ast

/** An initial algebra of arithmetic expressions. */
sealed trait Expr
case class Constant(value: Int) extends Expr
abstract class UnaryExpr(expr: Expr) extends Expr { require { expr != null } }
case class UMinus(expr: Expr) extends UnaryExpr(expr)
abstract class BinaryExpr(left: Expr, right: Expr) extends Expr { require { (left != null) && (right != null) } }
case class Plus(left: Expr, right: Expr) extends BinaryExpr(left, right)
case class Minus(left: Expr, right: Expr) extends BinaryExpr(left, right)
case class Times(left: Expr, right: Expr) extends BinaryExpr(left, right)
case class Div(left: Expr, right: Expr) extends BinaryExpr(left, right)
case class Mod(left: Expr, right: Expr) extends BinaryExpr(left, right)

case class Variable(value: String) extends Expr { require { value != null } }
case class Block(expressions: Expr*) extends Expr
case class Cond(guard: Expr, thenBranch: Expr, elseBranch: Expr) extends Expr // the elsebranch can be a block of zero elements
case class Loop(guard: Expr, body: Expr) extends Expr
case class Assign(left: Expr, right: Expr) extends BinaryExpr(left, right) // maybe

// case class Select(expr: Expr, field: Expr) extends Expr
// case class Struct(map: Expr) extends Expr
