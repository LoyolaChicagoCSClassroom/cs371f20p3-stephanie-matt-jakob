package edu.luc.cs.laufer.cs371.expressions

object TestFixtures {

  import ast._

  val EOL = scala.util.Properties.lineSeparator

  val assignment = 
    Assign(
      Variable("x"),
      Constant(3)
    )

  val assignmentString = "x = 3;"

  val assignmentUnpars = "x = 3;"

  val whileAST = 
    Loop(
      Constant(0),
      Assign(
        Variable("y"),
        Constant(3)
      ),
    )

  val whileString = "while (0) { y = 3; }"

  val whileUnpars = "while (0){" + EOL + "  y = 3;" + EOL + "}"

  val conditionAST = 
    Cond(
      Constant(1),
      Block(
        Assign(
          Variable("x"),
          Constant(2)
        ),
      ),
      Block(
        Assign(
          Variable("x"),
          Constant(3)
        ),
      ),
    )

  val conditionString = "if (1) { x = 2; } else { x = 3; }"

  val conditionUnpars = "if (1){" + EOL + "  x = 2;" + EOL + "} else{" + EOL + "  x = 3;" + EOL + "}"

  val blockAST = 
    Block(
      Assign(
        Variable("r"),
        Plus(
          Variable("r"),
          Variable("x")
        ),
      ),
      Assign(
        Variable("y"),
        Plus(
          Variable("y"),
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
            Assign(
              Variable("x"),
              Constant(3)
            ),
            Assign(
              Variable("y"),
              Constant(5)
            ),
            Block(
              Assign(
                Variable("xy"),
                Constant(88)
              ),
            ),
          ),
        ),
      ),
      Block(),
    )

  val complex1string2 = "if(-3+4+5*6){while(0){x=3;y=5;{xy=88;}}}"
}