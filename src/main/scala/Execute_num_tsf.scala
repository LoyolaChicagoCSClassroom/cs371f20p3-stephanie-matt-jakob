package edu.luc.cs.laufer.cs371.expressions
import scala.collection.mutable.{ Map => MMap }
import scala.util.{ Try, Success, Failure }
import ast._

/** An interpreter for expressions and statements. */
object Execute_num_tsf {

  // adding the sealed trait Value and its Num case class
  sealed trait Value
  case class Num(value: Int) extends Value

  // adding the two main types
  type Store = MMap[String, Num]
  type Result = Try[Num]

  /** functions with working with store and Variable. */
  // checks to see if the name is in the memory hash map aka store
  // returns true or false
  def storeHasVariable(store: Store)(s: Expr): Boolean = s match {
    case Variable(name) => store.contains(name)
    case _ => false
  }

  // puts a variable into the hashmap has a type Num
  // the function has a dummy return variable of Num, the important part is that it is now in the store
  def putVariableinStore(store: Store)(s: Expr)(num: Result): Num = s match {
    case Variable(name) => { store.put(name.toString(), getNumfromResult(num)); getNumfromResult(num) }
  }

  // gets the name of the item inside the Variable wrapper
  def getVariablefromStore(store: Store)(s: Expr): String = s match {
    case Variable(name) => name
  }

  // gets the int from inside a Num wrapper
  def getIntfromNum(num: Result): Int = num match {
    case Success(Num(x)) => x
  }

  def getNumfromResult(num: Result): Num = num match {
    case Success(Num(x)) => Num(x)
  }

  // get a sucess or fail value from trying to get something from the store
  def lookup(store: Store)(name: String): Result = {
    val scenario = Try(store(name))
    scenario match {
      case Failure(exception) => Failure(new NoSuchFieldException(name))
      case Success(name) => Success(name)
    }
  }

  // apply method to retrieve an int from ast
  def apply(store: Store)(s: Expr): Result = s match {
    case Constant(value) => Success(Num(value))
    case Plus(left, right) => Success(Num(getIntfromNum(apply(store)(left)) + getIntfromNum(apply(store)(right))))
    case Minus(left, right) => Success(Num(getIntfromNum(apply(store)(left)) - getIntfromNum(apply(store)(right))))
    case Times(left, right) => Success(Num(getIntfromNum(apply(store)(left)) * getIntfromNum(apply(store)(right))))
    case Div(left, right) => Success(Num(getIntfromNum(apply(store)(left)) / getIntfromNum(apply(store)(right))))
    case Variable(name) => lookup(store)(name)
    case Assign(left, right) => {
      if (!storeHasVariable(store)(left)) {
        val rvalue_1 = apply(store)(right)
        putVariableinStore(store)(left)(rvalue_1)
      } else {
        val rvalue = apply(store)(right)
        val lvalue = getVariablefromStore(store)(left)
        store.put(lvalue, getNumfromResult(rvalue))
      }
      Success(Num(0))
    }
    case Block(ss @ _*) => {
      // ss.foldLeft(Success(Num(0)))((c, s) => apply(store)(s))
      val i = ss.iterator
      var result: Num = Num(0)
      while (i.hasNext) {
        apply(store)(i.next()) match {
          case Success(r) => result = r
          case f @ Failure(_) => return f
        }
      }
      Success(result)
    }
    case Loop(guard, body) => {
      var gvalue = apply(store)(guard)
      while (gvalue != Success(Num(0))) {
        apply(store)(body)
        gvalue = apply(store)(guard)
      }
      Success(Num(0))
    }
  }

}
