package edu.luc.cs.laufer.cs371.expressions

object TestFixtures {

  import ast._

  val EOL = scala.util.Properties.lineSeparator

  val assignment = 
    MultiAssign(
      List("x"),
      Constant(3)
    )

  val assignmentString = "x = 3;"

  val assignmentUnpars = "x = 3;"

  val whileAST = 
    Loop(
      Constant(0),
      Block(
        MultiAssign(
        List("y"),
        Constant(3)
        ),
      )
    )

  val whileString = "while (0) { y = 3; }"

  val whileUnpars = "while (0){" + EOL + "  y = 3;" + EOL + "}"

  val whileMapString = "x = 2; y = 3; r = 0; while (y) { r = r + x ; y = y - 1; }"

  val whileMap = "HashMap(r -> Num(6), x -> Num(2), y -> Num(0))"

  val conditionAST = 
    Cond(
      Constant(1),
      Block(
        MultiAssign(
          List("x"),
          Constant(2)
        ),
      ),
      Block(
        MultiAssign(
          List("x"),
          Constant(3)
        ),
      ),
    )

  val conditionString = "if (1) { x = 2; } else { x = 3; }"

  val conditionUnpars = "if (1){" + EOL + "  x = 2;" + EOL + "} else{" + EOL + "  x = 3;" + EOL + "}"

  val condMapString = "x = 2; y = 3; r = 0; if (y) { r = r + x ; y = y + 3; }"

  val condMap = "HashMap(r -> Num(2), x -> Num(2), y -> Num(6))"

  val blockAST = 
    Block(
      MultiAssign(
        List("r"),
        Plus(
          Select("r"),
          Select("x")
        ),
      ),
      MultiAssign(
        List("y"),
        Plus(
          Select("y"),
          Constant(1)
        ),
      ),
    )

  val blockString = "{ r = r + x; y = y + 1 ; }"

  val blockUnpars = "{" + EOL + "  r = (r + x);" + EOL + "  y = (y + 1);" + EOL + "}"

  val complex1 =
    Div(
      Minus(
        Plus(
          Constant(1),
          Constant(2)
        ),
        Times(
          Constant(3),
          Constant(4)
        )
      ),
      Constant(5)
    )

  val complex1string = "((1 + 2) - (3 * 4)) / 5"

  val complex2 =
    Cond(
      Plus(
        Plus(
          Constant(-3),
          Constant(4)
        ),
        Times(
          Constant(5),
          Constant(6)
        )
      ),
      Block(
        Loop(
          Constant(0),
          Block(
            MultiAssign(
              List("x"),
              Constant(3)
            ),
            MultiAssign(
              List("y"),
              Constant(5)
            ),
            Block(
              MultiAssign(
                List("xy"),
                Constant(88)
              ),
            ),
          ),
        ),
      ),
      Block(),
    )

  val complex1string2 = "if(-3+4+5*6){while(0){x=3;y=5;{xy=88;}}}"

  val assignmentMapString =  "y = 1;"

  val assignmentMap = "HashMap(y -> Num(1))"

  val blockMapString =  "{x = 0; y = 1;}"

  val blockMap = "HashMap(x -> Num(0), y -> Num(1), r -> Num(5))"

  // Test fixture for if variable hasn't been assigned
  val throwErrorString = "z;"

  val throwErrorOutput = "Failure(java.lang.NoSuchFieldException: z)"

  val structString = "x = { a: 3 + 4, b: 5 + 6 };"

  val struct = "Block(MultiAssign((x),Struct(Map(a -> Plus(Constant(3),Constant(4)), b -> Plus(Constant(5),Constant(6)))))),"
 
  val selectFieldString = "x.a;"

  val selectField = "Block(Select(x.a))"
 
  val assignFieldString = "x.a = 9;" 

  val assignField = "HashMap(x -> Ins(HashMap(a -> Num(9), b -> Num(11))))"

  val fieldAddToStructString = "x.c = 13;"
  
  val fieldAddToStruct = "HashMap(x -> Ins(HashMap(a -> Num(9), b -> Num(11), c -> Num(13))))"

}
