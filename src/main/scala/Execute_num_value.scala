package edu.luc.cs.laufer.cs371.expressions
import scala.collection.mutable.{ Map => MMap }
import scala.util.{ Try, Success, Failure }
import ast._

/** An interpreter for expressions and statements. */
object Execute_num_value {

  // adding the two main types
  type Instance = MMap[String, Value]
  type Store = Instance

  // adding the sealed trait Value and its Num case class
  sealed trait Value
  case class Num(value: Int) extends Value
  case class Ins(value: Instance) extends Value

  // the result of a successful or fauled computation 
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
  def putVariableinStore(store: Store)(s: Expr)(num: Result): Value = s match {
    case Variable(name) => { store.put(name.toString(), getNumfromResult(num)); getNumfromResult(num) }
  }

  // gets the name of the item inside the Variable wrapper
  def getVariablefromStore(store: Store)(s: Expr): String = s match {
    case Variable(name) => name
  }

  // gets the int from inside a Num wrapper
  def getIntfromNum(num: Value): Int = num match {
    case Num(x) => x
  }

  // num
  def getNumfromResult(num: Result): Value = num match {
    case Success(Num(x)) => Num(x)
    //  case Success(Ins(result)) => Ins(result)
  }

  // // field
  // def getExprFromField(field: Expr): Expr = field match {
  //   case Field(name, value) => value
  // }

  // // field 
  // def getNamefromField(field: Expr): String = field match {
  //   case Field(name, value) => name
  // }

  /** Looks up a variable in memory. */
  def lookup(store: Store)(name: String): Result =
    store.get(name).fold {
      Failure(new NoSuchFieldException(name)): Result
    } {
      Success(_)
    }

  def binOp(store: Store, left: Expr, right: Expr, op: (Int, Int) => Int): Result =
    for {
      Num(l) <- apply(store)(left);
      Num(r) <- apply(store)(right)
    } yield Num(op(l, r))

  // apply method to retrieve an int from ast
  def apply(store: Store)(s: Expr): Result = s match {
    case Constant(value) => Success(Num(value))
    case Plus(left, right) => binOp(store, left, right, _ + _)
    case Minus(left, right) => binOp(store, left, right, _ - _)
    case Times(left, right) => binOp(store, left, right, _ * _)
    case Div(left, right) => binOp(store, left, right, _ / _)
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
      val i = ss.iterator
      // var result: Num = Num(0)
      var result: Int = 0
      while (i.hasNext) {
        apply(store)(i.next()) match {
          case Success(r) => result = getIntfromNum(r)
          case f @ Failure(_) => return f
        }
      }
      Success(Num(result))
    }
    case Loop(guard, body) => {
      var gvalue = apply(store)(guard)
      while (gvalue != Success(Num(0))) {
        apply(store)(body)
        gvalue = apply(store)(guard)
      }
      Success(Num(0))
    }
    case Cond(x,y,z) => {
      if (apply(store)(x) != Success(Num(0))) {
        apply(store)(y)
      }
      else{
        apply(store)(z)
      }
    }
    // case Struct(map @ _*) => {
    //   val i = map.iterator
    //   var result: Instance = MMap[String, Value]()
    //   while (i.hasNext) {
    //     var curr_name = getNamefromField(i.next())
    //     var curr_exp = getExprFromField(i.next()) 
    //     apply(store)(curr_exp) match {
    //       case Success(r) => result.put(curr_name, r)
    //       case f @ Failure(_) => return f
    //     }
    //   }
    //   Success(Ins(result))
    //  }
  }

}
