import scala.collection.mutable

/**
 * Created by shepatnaik on 08/01/16.
 */

object LispParser {

 val globalScope : Scope = mutable.Map(
    "+" -> LispFunctionDefinition(List("n", "m"), LispBuiltInFunctionPlus()),
    "-" -> LispFunctionDefinition(List("n", "m"), LispBuiltInFunctionMinus()),
    "*" -> LispFunctionDefinition(List("n", "m"), LispBuiltInFunctionMultiply()),
    "/" -> LispFunctionDefinition(List("n", "m"), LispBuiltInFunctionDivide()),
    "def" -> LispFunctionDefinition(List("name", "exp"), LispBuiltInFunctionDefine())
  )

  def evaluateExpression(expression: LispExpression) = expression.evaluate(globalScope)

  type Scope = mutable.Map[String, LispFunctionDefinition]

  trait LispExpression {
    def evaluate(scope : Scope) : LispExpression
  }

  object LispNil extends LispExpression {
    override def evaluate(scope : Scope) = this
  }

  case class LispNumber(value: Int) extends LispExpression {
    override def evaluate(scope : Scope) = this
  }

  case class LispString(value: String) extends LispExpression {
    override def evaluate(scope : Scope) = this
  }

  case class LispList(l: List[Any]) extends LispExpression {
    override def evaluate(scope : Scope) = this
  }

  case class LispSymbol(name: String) extends LispExpression {
    override def evaluate(scope : Scope) = this
  }

  case class LispVariable(name: String) extends LispExpression {
    override def evaluate(scope : Scope) = scope.get(name).get
  }

  case class LispFunctionDefinition(arguments: List[String], expression: LispExpression) extends LispExpression {
    override def evaluate(scope : Scope) = this
  }

  case class LispFunction(name: String, arguments: List[LispExpression]) extends LispExpression {

    private def argsToScope(scope: Scope, argNames: List[String], args: List[LispExpression]): Scope = {
      val immutable = argNames.zip(args.map(
        (x : LispExpression) =>
          LispFunctionDefinition(List(), x.evaluate(scope))
      ))

      scala.collection.mutable.Map(immutable: _*) ++ scope
    }

    override def evaluate(scope : Scope) = {
      val thisElement : Option[LispFunctionDefinition] = scope.get(name)
      // Create scope from passed arguments

      val functionScope : Scope = argsToScope(scope, thisElement.get.arguments, arguments)

      // Evaluate function
      thisElement.map(_.expression.evaluate(functionScope)).get
    }
  }

  case class LispBuiltInFunctionPlus() extends LispExpression {
    override def evaluate(scope : Scope) = LispNumber(
      scope.get("n").get.expression.asInstanceOf[LispNumber].value +
        scope.get("m").get.expression.asInstanceOf[LispNumber].value)
  }

  case class LispBuiltInFunctionMinus() extends LispExpression {
    override def evaluate(scope : Scope) = LispNumber(
      scope.get("n").get.expression.asInstanceOf[LispNumber].value -
        scope.get("m").get.expression.asInstanceOf[LispNumber].value)
  }

  case class LispBuiltInFunctionMultiply() extends LispExpression {
    override def evaluate(scope : Scope) = LispNumber(
      scope.get("n").get.expression.asInstanceOf[LispNumber].value *
        scope.get("m").get.expression.asInstanceOf[LispNumber].value)
  }

  case class LispBuiltInFunctionDivide() extends LispExpression {
    override def evaluate(scope : Scope) = LispNumber(
      scope.get("n").get.expression.asInstanceOf[LispNumber].value /
        scope.get("m").get.expression.asInstanceOf[LispNumber].value)
  }

  case class LispBuiltInFunctionDefine() extends LispExpression {
    override def evaluate(scope : Scope) = {
      // Side effect of updating Scope
      scope.put(
        scope.get("name").get.expression.asInstanceOf[LispString].value,
        LispFunctionDefinition(List(), scope.get("exp").get.expression.evaluate(scope))
      )

      println(scope)

      LispNil
    }
  }

}
