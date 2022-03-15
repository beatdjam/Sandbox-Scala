package evaluator

import `object`.{Integer, Object}
import ast.{
  Expression,
  ExpressionStatement,
  IntegerLiteral,
  Node,
  Program,
  Statement
}

object Evaluator {
  def eval(node: Node): Option[Object] = {
    node match {
      case Program(statements) => evalStatements(statements)
      case ExpressionStatement(_, Some(expression)) =>
        eval(expression)
      case IntegerLiteral(_, value) =>
        Some(Integer(value))
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
}
