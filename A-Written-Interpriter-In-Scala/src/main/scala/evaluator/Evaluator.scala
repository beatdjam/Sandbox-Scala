package evaluator

import `object`.{Bool, Integer, Null, Object}
import ast.{
  BlockStatement,
  BooleanExpression,
  Expression,
  ExpressionStatement,
  IfExpression,
  InfixExpression,
  IntegerLiteral,
  Node,
  PrefixExpression,
  Program,
  Statement
}

object Evaluator {
  private val TRUE = Bool(true)
  private val FALSE = Bool(false)
  private val NULL = Null()

  def eval(node: Node): Option[Object] = {
    node match {
      case Program(statements) => evalStatements(statements)
      case ExpressionStatement(_, Some(expression)) =>
        eval(expression)
      case IntegerLiteral(_, value) =>
        Some(Integer(value))
      case BooleanExpression(_, value) =>
        Some(nativeBoolToBool(value))
      case PrefixExpression(_, operator, right) =>
        evalPrefixExpression(operator, eval(right))
      case InfixExpression(_, left, operator, right) =>
        evalInfixExpression(operator, eval(left), eval(right))
      case BlockStatement(_, statements) =>
        evalStatements(statements)
      case IfExpression(_, condition, consequence, alternative) =>
        eval(condition).flatMap { condition =>
          if (isTruthy(condition)) eval(consequence)
          else if (alternative.isDefined) alternative.flatMap(eval(_))
          else Some(NULL)
        }
      case _ => None
    }
  }

  private def evalStatements(
      statements: Seq[Option[Statement]]
  ): Option[Object] = {
    val result = statements.flatMap { _.flatMap(eval(_)) }
    result.headOption
  }

  private def nativeBoolToBool(input: Boolean): Bool =
    if (input) TRUE else FALSE

  private def evalPrefixExpression(
      operator: String,
      right: Option[Object]
  ): Option[Object] = {
    def evalMinusPrefixOperatorExpression(right: Option[Object]) = right match {
      case Some(Integer(value)) => Some(Integer(-value))
      case _                    => None
    }

    def evalBangOperatorExpression(right: Option[Object]) = right match {
      case Some(TRUE)  => Some(FALSE)
      case Some(FALSE) => Some(TRUE)
      case Some(NULL)  => Some(TRUE)
      case _           => Some(FALSE)
    }

    operator match {
      case "!" => evalBangOperatorExpression(right)
      case "-" => evalMinusPrefixOperatorExpression(right)
      case _   => None
    }
  }
  private def evalInfixExpression(
      operator: String,
      left: Option[Object],
      right: Option[Object]
  ): Option[Object] = {
    def evalIntegerInfixExpression(
        operator: String,
        left: Int,
        right: Int
    ): Option[Object] = operator match {
      case "+" => Some(Integer(left + right))
      case "-" => Some(Integer(left - right))
      case "*" => Some(Integer(left * right))
      case "/" => Some(Integer(left / right))
      case "<" => Some(Bool(left < right))
      case ">" => Some(Bool(left > right))
      case _   => None
    }

    (operator, left, right) match {
      case ("==", _, _) =>
        Some(nativeBoolToBool(left == right))
      case ("!=", _, _) =>
        Some(nativeBoolToBool(left != right))
      case (_, Some(Integer(leftValue)), Some(Integer(rightValue))) =>
        evalIntegerInfixExpression(operator, leftValue, rightValue)
      case _ => None
    }
  }

  private def isTruthy(obj: Object): Boolean = obj match {
    case NULL  => false
    case TRUE  => true
    case FALSE => false
    case _     => true
  }
}
