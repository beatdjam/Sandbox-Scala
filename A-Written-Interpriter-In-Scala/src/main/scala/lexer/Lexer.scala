package lexer

import token.{
  ASSIGN,
  ASTERISK,
  BANG,
  COMMA,
  EOF,
  GT,
  ILLEGAL,
  INT,
  LBRACE,
  LPAREN,
  LT,
  MINUS,
  PLUS,
  RBRACE,
  RPAREN,
  SEMICOLON,
  SLASH,
  Token
}

case class Lexer private (input: String) {
  private var readPosition: Int = 0

  private def ch =
    if (readPosition < input.length) Some(input(readPosition).toString)
    else None

  def nextToken(): Token = {
    skipWhiteSpace()

    ch.map {
      case ch @ ("=" | "+" | "-" | "!" | "/" | "*" | "<" | ">") =>
        readChar()
        Token.fromOperatorLiteral(ch)
      case ch @ ("," | ";" | "(" | ")" | "{" | "}") =>
        readChar()
        Token.fromDelimiterLiteral(ch)
      case str if isLetter(str) =>
        val literal = read(isLetter)
        Token.fromLiteral(literal)
      case str if isDigit(str) =>
        val literal = read(isDigit)
        Token(INT, literal)
      case str =>
        readChar()
        Token(ILLEGAL, str)
    }.getOrElse {
      readChar()
      Token(EOF, "")
    }
  }

  private def readChar(): Unit = {
    readPosition = readPosition + 1
  }

  private def skipWhiteSpace(): Unit = {
    def isWhiteSpace = ch.exists(_ match {
      case " " | "\n" | "\t" | "\r" => true
      case _                        => false
    })
    while (isWhiteSpace) readChar()
  }

  private def isLetter(ch: String): Boolean = {
    val list = ('a' to 'z') ++ ('A' to 'Z')
    ch.forall(list.contains) || ch == "_"
  }

  private def isDigit(ch: String): Boolean = {
    val list = '0' to '9'
    ch.forall(list.contains)
  }

  private def read(fn: String => Boolean): String = {
    val currentPosition = readPosition
    while (ch.exists(fn)) readChar()
    input.substring(currentPosition, readPosition)
  }
}

object Lexer {
  def from(input: String): Lexer = Lexer(input)
}
