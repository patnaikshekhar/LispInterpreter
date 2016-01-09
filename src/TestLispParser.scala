/**
 * Created by shepatnaik on 08/01/16.
 */

import LispParser._

object TestLispParser extends App {
  println(evaluateExpression(LispString("Test")))

  println(evaluateExpression(LispFunction("+", List(LispNumber(1), LispNumber(2)))))

  println(evaluateExpression(LispFunction("+",
    List(
      LispFunction("*",
        List(LispNumber(9), LispNumber(3))
      ),
      LispNumber(2)
    )
  )))

  println(evaluateExpression(
    LispFunction("def",
      List(
        LispString("Test"),
        LispNumber(1)
      )
    )
  ))

  println(globalScope)

//  println(evaluateExpression(
//      LispVariable("Test")
//    )
//  )
}
