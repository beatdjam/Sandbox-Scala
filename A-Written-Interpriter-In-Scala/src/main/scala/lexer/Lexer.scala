package lexer

import token.{EOF, EQ, ILLEGAL, INT, NOT_EQ, STRING, Token}

case class Lexer private (input: String) {
  private var readPosition: Int = 0

  private def ch =
    if (readPosition < input.length) Some(input(readPosition).toString)
    else None

  private def peekCh =
    if (readPosition + 1 < input.length) Some(input(readPosition + 1).toString)
    else None

  def nextToken(): Token = {
    skipWhiteSpace()

    ch.map {
      case "=" if peekCh.contains("=") =>
        readChar(2)
        Token(EQ, "==")
      case "!" if peekCh.contains("=") =>
        readChar(2)
        Token(NOT_EQ, "!=")
      case ch @ ("=" | "+" | "-" | "!" | "/" | "*" | "<" | ">") =>
        readChar()
        Token.fromOperatorLiteral(ch)
      case ch @ ("," | ":" | ";" | "(" | ")" | "{" | "}" | "[" | "]") =>
        readChar()
        Token.fromDelimiterLiteral(ch)
      case "\"" =>
        readChar()
        val literal = read(isString)
        readChar()
        Token(STRING, literal)
      case ch if isLetter(ch) =>
        val literal = read(isLetter)
        Token.fromLiteral(literal)
      case ch if isDigit(ch) =>
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

  private def readChar(count: Int = 1): Unit = {
    readPosition = readPosition + count
  }

  private def skipWhiteSpace(): Unit = {
    def isWhiteSpace = ch.exists(_ match {
      case " " | "\n" | "\t" | "\r" => true
      case _                        => false
    })
    while (isWhiteSpace) readChar()
  }

  private def isString(ch: String): Boolean = !ch.contains("\"")

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
