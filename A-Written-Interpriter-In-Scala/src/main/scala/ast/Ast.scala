package ast

import token.Token

trait Node {
  def tokenLiteral(): String
  def getString: String
}

trait Statement extends Node {
  val token: Token
  override def tokenLiteral(): String = token.literal
}
trait Expression extends Node {
  val token: Token
  override def tokenLiteral(): String = token.literal
}

case class Program(statements: Seq[Option[Statement]]) extends Node {
  override def tokenLiteral(): String =
    statements.headOption.flatMap(_.map(_.tokenLiteral())).getOrElse("")

  override def getString: String =
    statements.flatMap(_.map(_.getString)).mkString("\n")
}

case class Identifier(token: Token, value: String) extends Expression {
  override def getString: String = value
}
case class LetStatement(
    token: Token,
    name: Identifier,
    value: Option[Expression]
) extends Statement {
  override def getString: String =
    s"${tokenLiteral()} ${name.getString} = ${value.map(_.getString).getOrElse("")};"
}

case class ReturnStatement(
    token: Token,
    returnValue: Option[Expression]
) extends Statement {
  override def getString: String =
    s"${tokenLiteral()} ${returnValue.map(_.getString).getOrElse("")};"
}

case class ExpressionStatement(
    token: Token,
    expression: Option[Expression]
) extends Statement {
  override def getString: String = expression.map(_.getString).getOrElse("")
}
