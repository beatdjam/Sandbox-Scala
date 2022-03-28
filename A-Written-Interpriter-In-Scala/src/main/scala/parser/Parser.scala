package parser

import ast.{
  ArrayLiteral,
  BlockStatement,
  BooleanExpression,
  CallExpression,
  Expression,
  ExpressionStatement,
  FunctionLiteral,
  HashLiteral,
  Identifier,
  IfExpression,
  IndexExpression,
  InfixExpression,
  IntegerLiteral,
  LetStatement,
  PrefixExpression,
  Program,
  ReturnStatement,
  Statement,
  StringLiteral
}
import lexer.Lexer
import parser.Priority.LOWEST
import token.{
  ASSIGN,
  ASTERISK,
  BANG,
  COLON,
  COMMA,
  ELSE,
  EOF,
  EQ,
  FALSE,
  FUNCTION,
  GT,
  IDENT,
  IF,
  INT,
  LBRACE,
  LBRACKET,
  LET,
  LPAREN,
  LT,
  MINUS,
  NOT_EQ,
  PLUS,
  RBRACE,
  RBRACKET,
  RETURN,
  RPAREN,
  SEMICOLON,
  SLASH,
  STRING,
  TRUE,
  Token,
  TokenType
}

import scala.collection.mutable.ListBuffer

object Priority extends Enumeration {
  val LOWEST, EQUALS, LESSGREATER, SUM, PRODUCT, PREFIX, CALL, INDEX = Value.id
}

case class Parser private (
    private val lexer: Lexer,
    private var curToken: Token,
    private var peekToken: Token
) {
  private val _errors = ListBuffer.empty[String]
  def errors: Seq[String] = _errors.toList

  private def nextToken(): Unit = {
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

  private def parseStatement(): Option[Statement] = {
    def parseLetStatement(): Option[LetStatement] = {
      val statement = curToken
      if (expectPeek(IDENT)) {
        val identifier = Identifier(curToken, curToken.literal)
        if (expectPeek(ASSIGN)) {
          nextToken()
          val value = parseExpression(LOWEST)
          if (peekTokenIs(SEMICOLON)) nextToken()
          Some(LetStatement(statement, identifier, value))
        } else None
      } else None
    }

    def parseReturnStatement(): Option[ReturnStatement] = {
      val current = curToken
      nextToken()
      val returnValue = parseExpression()
      if (peekTokenIs(SEMICOLON)) nextToken()
      Some(ReturnStatement(current, returnValue))
    }

    def parseExpressionStatement(): Option[ExpressionStatement] = {
      val current = curToken
      val expression = parseExpression()
      if (peekTokenIs(token.SEMICOLON)) nextToken()
      Some(ExpressionStatement(current, expression))
    }

    curToken.tokenType match {
      case LET    => parseLetStatement()
      case RETURN => parseReturnStatement()
      case _      => parseExpressionStatement()
    }
  }

  private def parseExpression(precedence: Int = LOWEST): Option[Expression] = {
    val precedences: Map[TokenType, Int] = Map(
      EQ -> Priority.EQUALS,
      NOT_EQ -> Priority.EQUALS,
      LT -> Priority.LESSGREATER,
      GT -> Priority.LESSGREATER,
      PLUS -> Priority.SUM,
      MINUS -> Priority.SUM,
      SLASH -> Priority.PRODUCT,
      ASTERISK -> Priority.PRODUCT,
      LPAREN -> Priority.CALL,
      LBRACKET -> Priority.INDEX
    )

    def peekPrecedence(): Int =
      precedences.getOrElse(peekToken.tokenType, Priority.LOWEST)

    def curPrecedence(): Int =
      precedences.getOrElse(curToken.tokenType, Priority.LOWEST)

    def parseIntegerLiteral(): Expression =
      IntegerLiteral(curToken, curToken.literal.toInt)

    def parseStringLiteral(): Expression =
      StringLiteral(curToken, curToken.literal)

    def parseBlockStatement(): BlockStatement = {
      val current = curToken
      val buf = ListBuffer.empty[Option[Statement]]
      nextToken()
      while (!curTokenIs(RBRACE) && !curTokenIs(EOF)) {
        buf.addOne(parseStatement())
        nextToken()
      }
      BlockStatement(current, buf.toList)
    }

    def parseFunctionLiteral(): Option[Expression] = {
      val current = curToken
      if (!expectPeek(LPAREN)) {
        None
      } else {
        val parameters = parseFunctionParameters()
        if (!expectPeek(LBRACE)) None
        else {
          val body = parseBlockStatement()
          Some(FunctionLiteral(current, parameters, body))
        }
      }
    }

    def parseBooleanExpression(): Expression =
      BooleanExpression(curToken, curToken.tokenType == TRUE)

    def parseGroupedExpression(): Option[Expression] = {
      nextToken()
      val exp = parseExpression()
      if (expectPeek(token.RPAREN)) exp
      else None
    }

    def parseIdentifier(): Expression =
      Identifier(curToken, curToken.literal)

    def parsePrefixExpression(): Option[Expression] = {
      val current = curToken
      if (current.tokenType == LBRACKET) {
        val expressions = parseExpressionList(RBRACKET)
        Some(ArrayLiteral(current, expressions))
      } else if (current.tokenType == LBRACE) {
        val buf = ListBuffer.empty[(Option[Expression], Option[Expression])]
        while (!peekTokenIs(RBRACE)) {
          nextToken()
          val key = parseExpression()
          if (expectPeek(COLON)) {
            nextToken()
            val value = parseExpression()
            if (peekTokenIs(RBRACE) || expectPeek(COMMA))
              buf.addOne((key, value))
          }
        }
        if (expectPeek(RBRACE)) {
          val pairs = buf.flatMap {
            case (Some(key), Some(value)) => Some(key -> value)
            case _                        => None
          }.toMap
          Some(HashLiteral(current, pairs))
        } else None
      } else {
        nextToken()
        parseExpression(Priority.PREFIX) match {
          case Some(right) =>
            Some(PrefixExpression(current, current.literal, right))
          case None => None
        }
      }
    }

    def parseInfixExpression(left: Expression): Option[Expression] = {
      val current = curToken
      val precedence = curPrecedence()
      if (curToken.tokenType == LPAREN) Some(parseCallExpression(left))
      else if (curToken.tokenType == LBRACKET) {
        nextToken()
        val index = parseExpression()
        if (expectPeek(RBRACKET)) index.map(IndexExpression(current, left, _))
        else None
      } else {
        nextToken()
        parseExpression(precedence) match {
          case Some(right) =>
            Some(InfixExpression(current, left, current.literal, right))
          case None => None
        }
      }
    }

    def parseIfExpression(): Option[Expression] = {
      if (!expectPeek(LPAREN)) None
      else {
        val current = curToken
        nextToken()
        val conditionOpt = parseExpression()
        conditionOpt match {
          case None | Some(_) if !expectPeek(RPAREN) || !expectPeek(LBRACE) =>
            None
          case Some(condition) =>
            val conSeq = parseBlockStatement()
            val alt = if (peekTokenIs(ELSE)) {
              nextToken()
              if (expectPeek(LBRACE)) Some(parseBlockStatement()) else None
            } else None
            Some(IfExpression(current, condition, conSeq, alt))
          case _ => None
        }
      }
    }

    def parseFunctionParameters(): Seq[Identifier] = {
      if (peekTokenIs(RPAREN)) {
        nextToken()
        Nil
      } else {
        nextToken()
        val buf = ListBuffer(Identifier(curToken, curToken.literal))
        while (peekTokenIs(COMMA)) {
          nextToken()
          nextToken()
          buf.addOne(Identifier(curToken, curToken.literal))
        }

        if (expectPeek(RPAREN)) buf.toList
        else Nil
      }
    }

    // NOTE: 先読みしたtokenがinfixのとき、セミコロンか現在の優先度以下の演算子がくるまでループする
    def getInfixExpression(leftExp: Option[Expression]): Option[Expression] = {
      leftExp.flatMap { left =>
        if (!peekTokenIs(token.SEMICOLON) && precedence < peekPrecedence()) {
          nextToken()
          val newLeft = parseInfixExpression(left)
          getInfixExpression(newLeft)
        } else Some(left)
      }
    }

    def parseCallExpression(leftExp: Expression): Expression = {
      val current = curToken
      val arguments = parseExpressionList(RPAREN)
      CallExpression(current, leftExp, arguments)
    }

    def parseExpressionList(endToken: TokenType): Seq[Expression] = {
      if (peekTokenIs(endToken)) {
        nextToken()
        Nil
      } else {
        nextToken()
        val buf = ListBuffer.empty[Expression]
        parseExpression().map(buf.addOne)

        while (peekTokenIs(COMMA)) {
          nextToken()
          nextToken()
          parseExpression().map(buf.addOne)
        }

        if (expectPeek(endToken)) buf.toList
        else Nil
      }
    }

    val leftExp = curToken.tokenType match {
      case IDENT                            => Some(parseIdentifier())
      case INT                              => Some(parseIntegerLiteral())
      case STRING                           => Some(parseStringLiteral())
      case LPAREN                           => parseGroupedExpression()
      case TRUE | FALSE                     => Some(parseBooleanExpression())
      case BANG | MINUS | LBRACKET | LBRACE => parsePrefixExpression()
      case IF                               => parseIfExpression()
      case FUNCTION                         => parseFunctionLiteral()
      case _ =>
        _errors.addOne(
          s"no prefix parse function for ${curToken.tokenType.token} found"
        )
        None
    }
    peekToken.tokenType match {
      case LBRACKET | LPAREN | PLUS | MINUS | SLASH | ASTERISK | EQ | NOT_EQ |
          LT | GT =>
        getInfixExpression(leftExp)
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
      s"expected next token to be ${tokenType.token}, got ${peekToken.literal} instead"
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
