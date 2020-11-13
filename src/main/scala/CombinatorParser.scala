package edu.luc.cs.laufer.cs371.expressions

import scala.util.parsing.combinator.JavaTokenParsers
import scala.util.matching.Regex
import ast._

object CombinatorParser extends JavaTokenParsers {

  /** expr ::= term { { "+" | "-" } term }* */
  // a statement has to have a ; at the end, differentiate it from an expression
  def expr: Parser[Expr] =
    term ~! rep(("+" | "-") ~ term) ^^ {
      case l ~ x => x.foldLeft(l) {
        case (res, "+" ~ r) => Plus(res, r)
        case (res, "-" ~ r) => Minus(res, r)
      }
    }

  /** term ::= factor { { "*" | "/" | "%" } factor }* */
  def term: Parser[Expr] =
    factor ~! rep(("*" | "/" | "%") ~ factor) ^^ {
      case l ~ x => x.foldLeft(l) {
        case (res, "*" ~ r) => Times(res, r)
        case (res, "/" ~ r) => Div(res, r)
        case (res, "%" ~ r) => Mod(res, r)
      }
    }

  // // assignment  ::= ident "=" expression ";"
  // def assignment: Parser[Expr] =
  //   factor ~! rep("=" ~ ";") ^^ {
  //     case l ~ x => x.foldLeft(l) {
  //       // if ((new Regex("[a-zA-Z] [a-zA-Z0-9]*") findAllIn l).filter(_.toString != " ").length > 0) {
  //       case (res, "=" ~ r) => Assign(res, r)
  //     }
  //   }

  // assignment  ::= ident "=" expression ";"
  // def assignment: Parser[Expr] =
  //   ident ~! rep("=" ~ ";") ^^ {
  //     case l ~ x => x.foldLeft(l) {
  //       case (res, "=" ~ r) => Assign(res, r)
  //     }
  //   }

  /** factor ::= wholeNumber | "+" factor | "-" factor | "(" expr ")" */
  /* need to add factor ::= ident | ... when ident ::= [a-zA-Z] [a-zA-Z0-9]* */
  // ^^ is a top level seperation, whatever is to the left of the character, the right is the semenatic action
  def factor: Parser[Expr] = (
    wholeNumber ^^ { case s => Constant(s.toInt) }
    | "+" ~> factor ^^ { case e => e }
    | "-" ~> factor ^^ { case e => UMinus(e) }
    | "(" ~ expr ~ ")" ^^ { case _ ~ e ~ _ => e }
    | ident ^^ { case i => Variable(i) }) // don't know what's supposed to go here, putting Variable returns an error
    // | ident ^^ { case i if (new Regex("[a-zA-Z] [a-zA-Z0-9]*") findAllIn i.mkString(",").length() > 0) => Variable(i) }) // don't know what's supposed to go here, putting Variable returns an error

  // /** statement ::= ident = expr | while (expr) statement | { statement , ... , statement } */
  // def statement: Parser[Expr] = (
  //   ident ~ "=" ~ expr ^^ { case s ~ _ ~ r => Assign(Variable(s), r) }
  // // | "while" ~ "(" ~> expr ~ ")" ~ statement ^^ { case g ~ _ ~ b => While(g, b) }
  // // | "{" ~> repsep(statement, ",") <~ "}" ^^ { case ss => Sequence(ss: _*) }
  // )
}
