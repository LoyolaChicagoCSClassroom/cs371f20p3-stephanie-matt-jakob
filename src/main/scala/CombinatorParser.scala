package edu.luc.cs.laufer.cs371.expressions

import scala.util.parsing.combinator.JavaTokenParsers
import scala.util.matching.Regex
import ast._

object CombinatorParser extends JavaTokenParsers {

  /** factor ::= wholeNumber | "+" factor | "-" factor | "(" expr ")" */
  /* need to add factor ::= ident | ... when ident ::= [a-zA-Z] [a-zA-Z0-9]* */
  // ^^ is a top level seperation, whatever is to the left of the character, the right is the semenatic action
  def factor: Parser[Expr] = (
    wholeNumber ^^ { case s => Constant(s.toInt) }
    | "+" ~> factor ^^ { case e => e }
    | "-" ~> factor ^^ { case e => UMinus(e) }
    | "(" ~ expr ~ ")" ^^ { case _ ~ e ~ _ => e }
    | struct
    | ident ^^ { case i => Variable(i) }
    | repsep(ident, ".") ^^ { case i => Select(i: _*) }
    )

  /** term ::= factor { { "*" | "/" | "%" } factor }* */
  def term: Parser[Expr] =
    factor ~! rep(("*" | "/" | "%") ~ factor) ^^ {
      case l ~ x => x.foldLeft(l) {
        case (res, "*" ~ r) => Times(res, r)
        case (res, "/" ~ r) => Div(res, r)
        case (res, "%" ~ r) => Mod(res, r)
      }
    }

  /** expr ::= term { { "+" | "-" } term }* */
  // a statement has to have a ; at the end, differentiate it from an expression
  def expr: Parser[Expr] =
    term ~! rep(("+" | "-") ~ term) ^^ {
      case l ~ x => x.foldLeft(l) {
        case (res, "+" ~ r) => Plus(res, r)
        case (res, "-" ~ r) => Minus(res, r)
      }
    }

  // field  ::= ident ":" expr
  def field: Parser[Expr] = (
    ident ~ ":" ~ expr ^^ { case s ~ _ ~ r => Struct(Map(s -> r)) })

  // assignment: ident "=" expression ";"
  def assignment: Parser[Expr] = (
    ident ~ "=" ~ expr ~ ";" ^^ { case s ~ _ ~ r ~ _ => Assign(Variable(s), r) })
  
  // assignment  ::= ident { "." ident }* "=" expression ";"
  def multi_assignment: Parser[Expr] = (
    repsep(ident, ".") ~ "=" ~ expr ~ ";" ^^ { case i ~ _ ~ e ~ _ => MultiAssign(i, e) })

  // conditional: "if" "(" expression ")" block [ "else" block ]
  def conditional: Parser[Expr] = (
    "if" ~ "(" ~> expr ~ ")" ~ block ~ opt("else" ~ block) ^^ {
      case g ~ _ ~ t ~ None => Cond(g, t, Block())
      case g ~ _ ~ t ~ Some(_ ~ e) => Cond(g, t, e)
    })

  // loop: "while" "(" expression ")" block
  def loop: Parser[Expr] = (
    "while" ~ "(" ~> expr ~ ")" ~ block ^^ { case g ~ _ ~ b => Loop(g, b) })

  // block : "{" statement* "}"
  def block: Parser[Expr] = (
    "{" ~> rep(statement) <~ "}" ^^ { case ss => Block(ss: _*) })

  // struct ::= "{" "}" | "{" field { "," field }* "}"
  def struct: Parser[Expr] = (
    "{" ~ "}" ^^ { case _ => Struct(Map()) }
    | "{" ~> repsep(field, ",") <~ "}" ^^ {
      case x => Struct(x.foldLeft(Map.empty[String, Expr]) { //start with initial empty map and fold left on maps
        case (res, r) => { res ++ r.asInstanceOf[Struct].map }
      })
    }
  )

  // statement: expression ";" | assignment | conditional | loop | block
  def statement: Parser[Expr] = (
    expr ~ ";" ^^ { case e ~ _ => e }
    | multi_assignment | conditional | loop | block | struct)

  // top level 
  def top_level: Parser[Expr] = (
    rep(statement) ^^ { case ss => Block(ss: _*) })
}
