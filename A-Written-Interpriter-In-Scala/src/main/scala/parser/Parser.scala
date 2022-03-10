package parser

import ast.{Identifier, LetStatement, Program, ReturnStatement, Statement}
import lexer.Lexer
import token.{ASSIGN, EOF, IDENT, LET, RETURN, SEMICOLON, Token, TokenType}

import scala.collection.mutable.ListBuffer

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
    curToken.tokenType match {
      case LET    => parseLetStatement()
      case RETURN => parseReturnStatement()
      case _      => None
    }
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

  def parseReturnStatement(): Option[ReturnStatement] = {
    val current = curToken
    while (!curTokenIs(SEMICOLON)) nextToken()
    Some(ReturnStatement(current, None))
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
