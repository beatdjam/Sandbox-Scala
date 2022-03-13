package parser

import ast.{
  BlockStatement,
  BooleanExpression,
  CallExpression,
  Expression,
  ExpressionStatement,
  FunctionLiteral,
  Identifier,
  IfExpression,
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
  LET,
  LPAREN,
  LT,
  MINUS,
  NOT_EQ,
  PLUS,
  RBRACE,
  RETURN,
  RPAREN,
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

  private def parseStatement(): Option[Statement] = {
    def parseLetStatement(): Option[LetStatement] = {
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

    def parseReturnStatement(): Option[ReturnStatement] = {
      val current = curToken
      while (!curTokenIs(SEMICOLON)) nextToken()
      Some(ReturnStatement(current, None))
    }

    def parseExpressionStatement(): Option[ExpressionStatement] = {
      val current = curToken
      val expression = parseExpression(Priority.LOWEST)

      if (peekTokenIs(token.SEMICOLON)) nextToken()

      Some(ExpressionStatement(current, expression))
    }

    curToken.tokenType match {
      case LET    => parseLetStatement()
      case RETURN => parseReturnStatement()
      case _      => parseExpressionStatement()
    }
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
      ASTERISK -> Priority.PRODUCT,
      LPAREN -> Priority.CALL
    )

    def peekPrecedence(): Int =
      precedences.getOrElse(peekToken.tokenType, Priority.LOWEST)

    def curPrecedence(): Int =
      precedences.getOrElse(curToken.tokenType, Priority.LOWEST)

    def parseIntegerLiteral(): Expression =
      IntegerLiteral(curToken, curToken.literal.toInt)

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
      if (curToken.tokenType == LPAREN) {
        Some(parseCallExpression(left))
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
        val conditionOpt = parseExpression(Priority.LOWEST)
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
        if (!peekTokenIs(token.SEMICOLON) && precedence < peekPrecedence) {
          nextToken()
          val newLeft = parseInfixExpression(left)
          getInfixExpression(newLeft)
        } else Some(left)
      }
    }

    def parseCallExpression(leftExp: Expression): Expression = {
      val current = curToken
      val arguments = parseCallArguments()
      CallExpression(current, leftExp, arguments)
    }

    def parseCallArguments(): Seq[Expression] = {
      if (peekTokenIs(RPAREN)) {
        nextToken()
        Nil
      } else {
        nextToken()
        val buf = ListBuffer.empty[Expression]
        parseExpression(Priority.LOWEST).map(buf.addOne)

        while (peekTokenIs(COMMA)) {
          nextToken()
          nextToken()
          parseExpression(Priority.LOWEST).map(buf.addOne)
        }

        if (expectPeek(RPAREN)) buf.toList
        else Nil
      }
    }

    val leftExp = curToken.tokenType match {
      case IDENT        => Some(parseIdentifier())
      case INT          => Some(parseIntegerLiteral())
      case LPAREN       => parseGroupedExpression()
      case TRUE | FALSE => Some(parseBooleanExpression())
      case BANG | MINUS => parsePrefixExpression()
      case IF           => parseIfExpression()
      case FUNCTION     => parseFunctionLiteral()
      case _ =>
        _errors.addOne(
          s"no prefix parse function for ${curToken.tokenType.token} found"
        )
        None
    }
    peekToken.tokenType match {
      case LPAREN | PLUS | MINUS | SLASH | ASTERISK | EQ | NOT_EQ | LT | GT =>
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
