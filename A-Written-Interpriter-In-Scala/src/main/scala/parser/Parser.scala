package parser

import ast.{
  Expression,
  ExpressionStatement,
  Identifier,
  IntegerLiteral,
  LetStatement,
  PrefixExpression,
  Program,
  ReturnStatement,
  Statement
}
import lexer.Lexer
import token.{
  ASSIGN,
  BANG,
  EOF,
  IDENT,
  INT,
  LET,
  MINUS,
  RETURN,
  SEMICOLON,
  Token,
  TokenType
}

import scala.collection.mutable.ListBuffer

object Priority extends Enumeration {
  val LOWEST, EQUALS, LESSGREATER, SUM, PRODUCT, PREFIX, CALL = Value
}

case class Parser private (
    lexer: Lexer,
    var curToken: Token,
    var peekToken: Token
) {
  type InfixParseFn = Expression => Expression
  var infixParseFns: Map[TokenType, InfixParseFn] = Map.empty

  private val _errors = ListBuffer.empty[String]
  def errors: Seq[String] = _errors.toList

  def nextToken(): Unit = {
    curToken = peekToken
    peekToken = lexer.nextToken()
  }

  def parseProgram(): Program = {
    val buf = ListBuffer.empty[Option[Statement]]
    while (!curTokenIs(EOF)) {
      buf.addOne(parseStatement())
      nextToken()
    }
    Program(buf.toList)
  }

  private def parseStatement(): Option[Statement] = curToken.tokenType match {
    case LET    => parseLetStatement()
    case RETURN => parseReturnStatement()
    case _      => parseExpressionStatement()
  }

  private def parseLetStatement(): Option[LetStatement] = {
    // letのステートメント
    val current = curToken

    // letに続くのはIdentifierのはず
    if (expectPeek(IDENT)) {
      val stmt =
        LetStatement(current, Identifier(curToken, curToken.literal), None)

      // let <Identifier> = ~略~ ;
      // になっているはず
      if (expectPeek(ASSIGN)) {
        // セミコロンまでスキップ
        while (!curTokenIs(SEMICOLON)) nextToken()
        Some(stmt)
      } else None
    } else None
  }

  private def parseReturnStatement(): Option[ReturnStatement] = {
    val current = curToken
    while (!curTokenIs(SEMICOLON)) nextToken()
    Some(ReturnStatement(current, None))
  }

  private def parseExpressionStatement(): Option[ExpressionStatement] = {
    val current = curToken
    val expression = parseExpression(Priority.LOWEST)

    if (peekTokenIs(token.SEMICOLON)) nextToken()

    Some(ExpressionStatement(current, expression))
  }

  private def parseExpression(priority: Priority.Value): Option[Expression] = {
    def parseIntegerLiteral(): Expression =
      IntegerLiteral(curToken, curToken.literal.toInt)

    def parseIdentifier(): Expression =
      Identifier(curToken, curToken.literal)

    def parsePrefixExpression(): Option[Expression] = {
      val current = curToken
      nextToken()

      parseExpression(Priority.PREFIX) match {
        case Some(right) =>
          Some(PrefixExpression(current, current.literal, right))
        case None => None
      }
    }

    curToken.tokenType match {
      case IDENT        => Some(parseIdentifier())
      case INT          => Some(parseIntegerLiteral())
      case BANG | MINUS => parsePrefixExpression()
      case _ =>
        _errors.addOne(
          s"no prefix parse function for ${curToken.tokenType.token} found"
        )
        None
    }
  }

  private def curTokenIs(tokenType: TokenType): Boolean =
    curToken.tokenType == tokenType
  private def peekTokenIs(tokenType: TokenType): Boolean =
    peekToken.tokenType == tokenType
  private def expectPeek(tokenType: TokenType): Boolean =
    if (peekTokenIs(tokenType)) {
      nextToken()
      true
    } else {
      peekError(tokenType)
      false
    }

  private def peekError(tokenType: TokenType) = {
    _errors.addOne(
      s"expected next token to be $tokenType, got $peekToken instead"
    )
  }
}

object Parser {
  def from(lexer: Lexer): Parser = {
    val cur = lexer.nextToken()
    val peek = lexer.nextToken()
    Parser(lexer, cur, peek)
  }
}
