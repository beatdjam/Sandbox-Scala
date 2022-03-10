package ast

import token.Token

trait Node {
  def tokenLiteral(): String
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
}

case class Identifier(token: Token, value: String) extends Expression
case class LetStatement(
    token: Token,
    name: Identifier,
    value: Option[Expression]
) extends Statement
