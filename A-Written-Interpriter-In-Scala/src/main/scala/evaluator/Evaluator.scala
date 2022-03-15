package evaluator

import `object`.{Bool, Integer, Null, Object}
import ast.{
  BooleanExpression,
  ExpressionStatement,
  IntegerLiteral,
  Node,
  Program,
  Statement
}

import scala.annotation.tailrec

object Evaluator {
  private val TRUE = Bool(true)
  private val FALSE = Bool(false)
  private val NULL = Null

  @tailrec
  def eval(node: Node): Option[Object] = {
    node match {
      case Program(statements) => evalStatements(statements)
      case ExpressionStatement(_, Some(expression)) =>
        eval(expression)
      case IntegerLiteral(_, value) =>
        Some(Integer(value))
      case BooleanExpression(_, value) =>
        Some(nativeBoolToBool(value))
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
}
