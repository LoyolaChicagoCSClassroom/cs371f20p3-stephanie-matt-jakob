package edu.luc.cs.laufer.cs371.expressions
import scala.collection.mutable.{ Map => MMap }
import scala.util.{ Try, Success, Failure }
import ast._

/** An interpreter for expressions and statements. */
object Execute_num {

  // adding the sealed trait Value and its Num case class
  sealed trait Value
  case class Num(value: Int) extends Value

  // adding the two main types
  type Store = MMap[String, Num]
  type Result = Try[Value]

  /** functions with working with store and Variable. */
  // checks to see if the name is in the memory hash map aka store
  // returns true or false
  def storeHasVariable(store: Store)(s: Expr): Boolean = s match {
    case Variable(name) => store.contains(name)
    case _ => false
  }

  // puts a variable into the hashmap has a type Num
  // the function has a dummy return variable of Num, the important part is that it is now in the store
  def putVariableinStore(store: Store)(s: Expr)(num: Num): Num = s match {
    case Variable(name) => { store.put(name.toString(), num); num }
  }

  // gets the name of the item inside the Variable wrapper
  def getVariablefromStore(store: Store)(s: Expr): String = s match {
    case Variable(name) => name
  }

  // gets the int from inside a Num wrapper
  def getIntfromNum(num: Num): Int = num match {
    case Num(x) => x
  }

  // apply method to retrieve an int from ast
  def apply(store: Store)(s: Expr): Num = s match {
    case Constant(value) => Num(value)
    case Plus(left, right) => Num(getIntfromNum(apply(store)(left)) + getIntfromNum(apply(store)(right)))
    case Minus(left, right) => Num(getIntfromNum(apply(store)(left)) - getIntfromNum(apply(store)(right)))
    case Times(left, right) => Num(getIntfromNum(apply(store)(left)) * getIntfromNum(apply(store)(right)))
    case Div(left, right) => Num(getIntfromNum(apply(store)(left)) / getIntfromNum(apply(store)(right)))
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
      Num(0)
    }
    case Block(ss @ _*) => {
      ss.foldLeft(Num(0))((c, s) => apply(store)(s))
    }
    case Loop(guard, body) => {
      var gvalue = apply(store)(guard)
      while (gvalue != Num(0)) {
        apply(store)(body)
        gvalue = apply(store)(guard)
      }
      Num(0)
    }
  }

}
