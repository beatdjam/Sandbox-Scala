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

case class BlockStatement(token: Token, statements: Seq[Option[Statement]])
    extends Expression {
  override def getString: String =
    statements.flatMap(_.map(_.getString)).mkString("\n")
}

case class Identifier(token: Token, value: String) extends Expression {
  override def getString: String = value
}

case class IntegerLiteral(token: Token, value: Int) extends Expression {
  override def getString: String = value.toString
}

case class PrefixExpression(token: Token, operator: String, right: Expression)
    extends Expression {
  override def getString: String = s"($operator${right.getString})"
}
case class InfixExpression(
    token: Token,
    left: Expression,
    operator: String,
    right: Expression
) extends Expression {
  override def getString: String =
    s"(${left.getString} $operator ${right.getString})"
}

case class BooleanExpression(token: Token, value: Boolean) extends Expression {
  override def getString: String = token.literal
}

case class IfExpression(
    token: Token,
    condition: Expression,
    consequence: BlockStatement,
    alternative: Option[BlockStatement]
) extends Expression {
  override def getString: String =
    s"""
       |if ${condition.getString} ${consequence.getString} ${alternative
      .map("else " + _.getString)
      .getOrElse("")}
       |""".stripMargin
}

case class FunctionLiteral(
    token: Token,
    parameters: Seq[Identifier],
    body: BlockStatement
) extends Expression {
  override def getString: String =
    s"${tokenLiteral()}(${parameters.mkString(", ")}) ${body.getString}"
}

case class CallExpression(
    token: Token,
    function: Expression,
    arguments: Seq[Expression]
) extends Expression {
  override def getString: String =
    s"${function.getString}(${arguments.map(_.getString).mkString(", ")})"
}
