package evaluator

import `object`.{Bool, Integer, Null, Object}
import ast.{
  BooleanExpression,
  Expression,
  ExpressionStatement,
  IntegerLiteral,
  Node,
  PrefixExpression,
  Program,
  Statement
}

import scala.annotation.tailrec

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
      case _ => None
    }
  }

  private def evalStatements(
      statements: Seq[Option[Statement]]
  ): Option[Object] = {
    val result = statements.flatMap { statementOpt =>
      statementOpt.flatMap(eval(_))
    }
    result.headOption
  }

  private def nativeBoolToBool(input: Boolean): Bool =
    if (input) TRUE else FALSE

  private def evalPrefixExpression(
      operator: String,
      right: Option[Object]
  ): Option[Object] = {
    def evalMinusPrefixOperatorExpression(right: Option[Object]) = {
      right match {
        case Some(Integer(value)) => Some(Integer(-value))
        case _                    => None
      }
    }

    def evalBangOperatorExpression(right: Option[Object]) = right match {
      case Some(TRUE)  => Some(FALSE)
      case Some(FALSE) => Some(TRUE)
      case Some(NULL)  => Some(TRUE)
      case _           => Some(FALSE)
    }

    operator match {
      case "!" =>
        evalBangOperatorExpression(right)
      case "-" =>
        evalMinusPrefixOperatorExpression(right)
    }
  }
}
