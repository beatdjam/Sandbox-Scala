package parser

import ast.{
  BooleanExpression,
  Expression,
  ExpressionStatement,
  Identifier,
  InfixExpression,
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
  ASTERISK,
  BANG,
  EOF,
  EQ,
  FALSE,
  GT,
  IDENT,
  INT,
  LET,
  LPAREN,
  LT,
  MINUS,
  NOT_EQ,
  PLUS,
  RETURN,
  SEMICOLON,
  SLASH,
  TRUE,
  Token,
  TokenType
}

import scala.collection.mutable.ListBuffer

object Priority extends Enumeration {
  val LOWEST, EQUALS, LESSGREATER, SUM, PRODUCT, PREFIX, CALL = Value.id
}

case class Parser private (
    lexer: Lexer,
    var curToken: Token,
    var peekToken: Token
) {
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

  private def parseExpression(precedence: Int): Option[Expression] = {
    val precedences: Map[TokenType, Int] = Map(
      EQ -> Priority.EQUALS,
      NOT_EQ -> Priority.EQUALS,
      LT -> Priority.LESSGREATER,
      GT -> Priority.LESSGREATER,
      PLUS -> Priority.SUM,
      MINUS -> Priority.SUM,
      SLASH -> Priority.PRODUCT,
      ASTERISK -> Priority.PRODUCT
    )
    def peekPrecedence(): Int =
      precedences.getOrElse(peekToken.tokenType, Priority.LOWEST)

    def curPrecedence(): Int =
      precedences.getOrElse(curToken.tokenType, Priority.LOWEST)

    def parseIntegerLiteral(): Expression =
      IntegerLiteral(curToken, curToken.literal.toInt)

    def parseBooleanExpression(): Expression =
      BooleanExpression(curToken, curToken.tokenType == TRUE)

    def parseGroupedExpression(): Option[Expression] = {
      nextToken()
      val exp = parseExpression(Priority.LOWEST)
      if (expectPeek(token.RPAREN)) exp
      else None
    }

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

    def parseInfixExpression(left: Expression): Option[Expression] = {
      val current = curToken
      val precedence = curPrecedence()
      nextToken()
      parseExpression(precedence) match {
        case Some(right) =>
          Some(InfixExpression(current, left, current.literal, right))
        case None => None
      }
    }

    // NOTE: 先読みしたtokenがinfixのとき、セミコロンか現在の優先度以下の演算子がくるまでループする
    def getExpression(leftExp: Option[Expression]): Option[Expression] = {
      leftExp.flatMap { left =>
        if (!peekTokenIs(token.SEMICOLON) && precedence < peekPrecedence) {
          nextToken()
          val newLeft = parseInfixExpression(left)
          getExpression(newLeft)
        } else Some(left)
      }
    }

    val leftExp = curToken.tokenType match {
      case IDENT        => Some(parseIdentifier())
      case INT          => Some(parseIntegerLiteral())
      case LPAREN       => parseGroupedExpression()
      case TRUE | FALSE => Some(parseBooleanExpression())
      case BANG | MINUS => parsePrefixExpression()
      case _ =>
        _errors.addOne(
          s"no prefix parse function for ${curToken.tokenType.token} found"
        )
        None
    }
    peekToken.tokenType match {
      case PLUS | MINUS | SLASH | ASTERISK | EQ | NOT_EQ | LT | GT =>
        getExpression(leftExp)
      case _ => leftExp
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
