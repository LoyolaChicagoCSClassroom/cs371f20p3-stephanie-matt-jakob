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

  def getIns(value: Value): Instance = value match {
    case Ins(x) => x
  }

  def putSeqInStore(store: Store)(s: Seq[String])(result: Result): Value = {
    var i = s.iterator.slice(0, s.length-1)
    if (s.length == 1) {
      store.put(s.last, getValuefromResult(result))
    } else {
      if (i.hasNext) {
        putSeqInStore(getIns(store(i.next())).asInstanceOf[Store])(s.slice(1, s.length))(result)
      }
    }
    getValuefromResult(result)
  }

  // gets the int from inside a Num wrapper
  def getIntfromNum(num: Value): Int = num match {
    case Num(x) => x
    // case Ins(y) => y
  }

  // num
  def getValuefromResult(num: Result): Value = num match {
    case Success(Num(x)) => Num(x)
    case Success(Ins(y)) => Ins(y)
  }

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
    // case Variable(name) => lookup(store)(name)
    case MultiAssign(left, right) => {
        val rvalue = apply(store)(right)
        putSeqInStore(store)(left)(rvalue)
        Success(Num(0))
    }
    case Select(names @ _*) => {
      var i = names.iterator.slice(0, names.length-1)
      var current_store = store 
      // all elements of the inputed list except for the last one are certainly a map, so get to the last map possible
      while (i.hasNext) {
          var current_i = i.next()
          if (current_store.contains(current_i)) {
            current_store = getIns(current_store(current_i)).asInstanceOf[Store]
        }
    }
    lookup(current_store)(names.last)
  }
    case Block(ss @ _*) => {
      val i = ss.iterator
      var result: Value = Num(0);
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
    case Cond(x,y,z) => {
      if (apply(store)(x) != Success(Num(0))) {
        apply(store)(y)
      }
      else{
        apply(store)(z)
      }
    }
    case Struct(map) => {
      var result: Instance = MMap[String, Value]()
      map foreach { case (key, value) => {
        apply(store)(value) match {
          case Success(r) => result.put(key, r)
          case f @ Failure(_) => return f
        }
      }}
      Success(Ins(result))
    }
  }
}


// archived

  // checks to see if the name is in the memory hash map aka store
  // returns true or false
  // used for repl testing: var s:Seq[String] = Seq("res1", "z", "x")
  // def storeHasSeq(store: Store)(s: Seq[String]): Boolean = {
  //   var isThere: Boolean = false 
  //   // if there was only one element this will return an empty list
  //   var i = s.iterator.slice(0, s.length-1)
  //   var current_store = store  //.asInstanceOf[Store]
  //   // all elements of the inputed list except for the last one are certainly a map, so get to the last map possible
  //   while (i.hasNext) {
  //       var current_i = i.next()
  //       if (current_store.contains(current_i)) {
  //         current_store = getIns(current_store(current_i)).asInstanceOf[Store]
  //         isThere = true 
  //       } else {
  //         isThere = false
  //       }  
  //     }
  //   isThere = current_store.contains(s.last)
  //   isThere 
  // }

  // puts variable in store, 
  // for repl testing:
  // var z = MMap("x"->1)
  // var res1 = MMap("z" -> z)
  // var store = MMap("res1" -> res1)
  //   def putSeqInStore(store: MMap[String, Any])(s: Seq[String])(result: Int): Int = {
  //   var i = s.iterator.slice(0, s.length-1)
  //   if (s.length == 1) {
  //     store.put(s.last, result) 
  //   } else {
  //     if (i.hasNext) {
  //       putSeqInStore( store(i.next()).asInstanceOf[MMap[String, Any]] )( s.slice(1, s.length) )( result )
  //     }
  //   }
  //   result
  // }


  //   // puts a variable into the hashmap has a type Num
  // // the function has a dummy return variable of Num, the important part is that it is now in the store
  // def putVariableinStore(store: Store)(s: Expr)(num: Result): Value = s match {
  //   case Variable(name) => { store.put(name.toString(), getValuefromResult(num)); getValuefromResult(num) }
  // }

  // // gets the name of the item inside the Variable wrapper
  // def getVariablefromStore(store: Store)(s: Expr): String = s match {
  //   case Variable(name) => name
  // }


    // case Assign(left, right) => {
    //   if (!storeHasVariable(store)(left)) {
    //     val rvalue_1 = apply(store)(right)
    //     putVariableinStore(store)(left)(rvalue_1)
    //   } else {
    //     val rvalue = apply(store)(right)
    //     val lvalue = getVariablefromStore(store)(left)
    //     store.put(lvalue, getValuefromResult(rvalue))
    //   }
    //   Success(Num(0))
    // }

  //     /** functions with working with store and Variable. */
  // // checks to see if the name is in the memory hash map aka store
  // // returns true or false
  // def storeHasVariable(store: Store)(s: Expr): Boolean = s match {
  //   case Variable(name) => store.contains(name)
  //   case _ => false
  // }