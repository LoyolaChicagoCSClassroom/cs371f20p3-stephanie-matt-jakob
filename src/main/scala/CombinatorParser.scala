package edu.luc.cs.laufer.cs371.expressions

import scala.util.parsing.combinator.JavaTokenParsers
import scala.util.matching.Regex
import ast._

object CombinatorParser extends JavaTokenParsers {

  /** factor ::= wholeNumber | "+" factor | "-" factor | "(" expr ")" */
  /* need to add factor ::= ident | ... when ident ::= [a-zA-Z] [a-zA-Z0-9]* */
  // ^^ is a top level seperation, whatever is to the left of the character, the right is the semenatic action
  def factor: Parser[Expr] = (
    // ident ~ "{" ~ "." ~ ident ^^ {case i ~ _ ~ _ ~ s => Select(i, s)}
    // | 
    wholeNumber ^^ { case s => Constant(s.toInt) }
    | "+" ~> factor ^^ { case e => e }
    | "-" ~> factor ^^ { case e => UMinus(e) }
    | "(" ~ expr ~ ")" ^^ { case _ ~ e ~ _ => e }
    | ident ^^ { case i => Variable(i) })

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

  // TODO: is field more like expression or assignment? 
  // field  ::= ident ":" expr
  // def field: Parser[Expr] = (
  //   ident ~ ":" ~ expr ^^ { case ss => what }
  // )

  // assignment: ident "=" expression ";"
  def assignment: Parser[Expr] = (
    ident ~ "=" ~ expr ~ ";" ^^ { case s ~ _ ~ r ~ _ => Assign(Variable(s), r) })

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
    "{" ~> rep(statement) <~ "}" ^^ { case ss => Block(ss: _*) }
  // | rep(statement) ^^ { case ss => Block(ss: _*) }
  )

  // statement: expression ";" | assignment | conditional | loop | block
  def statement: Parser[Expr] = (
    expr ~ ";" ^^ { case e ~ _ => e }
    | assignment | conditional | loop | block)

  // top level 
  def top_level: Parser[Expr] = (
    rep(statement) ^^ { case ss => Block(ss: _*) })
}
