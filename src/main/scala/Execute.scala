package edu.luc.cs.laufer.cs371.expressions
import scala.collection.mutable.{ Map => MMap }
import scala.util.{ Try, Success, Failure }
import ast._

/** An interpreter for expressions and statements. */
object Execute {

  type Store = MMap[String, Int]

  def apply(store: Store)(s: Expr): Int = s match {
    case Constant(value) => value
    case Plus(left, right) => apply(store)(left) + apply(store)(right)
    case Minus(left, right) => apply(store)(left) - apply(store)(right)
    case Times(left, right) => apply(store)(left) * apply(store)(right)
    case Div(left, right) => apply(store)(left) / apply(store)(right)
    case Variable(name) => { store.put(name.toString(), 0); 0 }
    case Assign(left, right) => {
      val lvalue = apply(store)(left)
      val rvalue = apply(store)(right)
      store.put(lvalue.toString(), rvalue)
      rvalue
    }
    case Block(statements @ _*) =>
      statements.foldLeft(0)((c, s) => apply(store)(s))
    // case Loop(guard, body) => {
    //   var gvalue = apply(store)(guard)
    //   while (gvalue != 0) {
    //     apply(store)(body)
    //     gvalue = apply(store)(guard)
    //   }
    //   0
    // }
  }

}
