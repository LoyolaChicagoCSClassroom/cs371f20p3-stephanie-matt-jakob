package edu.luc.cs.laufer.cs371.expressions
import scala.collection.mutable.{ Map => MMap }
import scala.util.{ Try, Success, Failure }
import ast._

/** An interpreter for expressions and statements. */
object Execute {

  type Store = MMap[String, Int]

  def storeHasVariable(store: Store)(s: Expr): Boolean = s match {
    case Variable(name) => store.contains(name)
    case _ => false
  }

  def putVariableinStore(store: Store)(s: Expr)(num: Int): Int = s match {
    case Variable(name) => { store.put(name.toString(), num); num }
  }

  def getVariablefromStore(store: Store)(s: Expr): String = s match {
    case Variable(name) => name
  }

  def apply(store: Store)(s: Expr): Int = s match {
    case Constant(value) => value
    case Plus(left, right) => apply(store)(left) + apply(store)(right)
    case Minus(left, right) => apply(store)(left) - apply(store)(right)
    case Times(left, right) => apply(store)(left) * apply(store)(right)
    case Div(left, right) => apply(store)(left) / apply(store)(right)
    case Variable(name) => store(name)
    case Assign(left, right) => {
      if (!storeHasVariable(store)(left)) {
        val rvalue_1 = apply(store)(right)
        putVariableinStore(store)(left)(rvalue_1)
      } else {
        val rvalue = apply(store)(right)
        val lvalue = getVariablefromStore(store)(left)
        store.put(lvalue, rvalue)
      }
      0
    }
    case Block(ss @ _*) =>
      ss.foldLeft(0)((c, s) => apply(store)(s))
    case Loop(guard, body) => {
      var gvalue = apply(store)(guard)
      while (gvalue != 0) {
        apply(store)(body)
        gvalue = apply(store)(guard)
      }
      0
    }
  }

}
