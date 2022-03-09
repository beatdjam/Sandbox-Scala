package lexer

import token.{
  ASSIGN,
  COMMA,
  EOF,
  ILLEGAL,
  INT,
  LBRACE,
  LPAREN,
  PLUS,
  RBRACE,
  RPAREN,
  SEMICOLON,
  Token
}

case class Lexer private (input: String) {
  private var position: Int = 0
  private var readPosition: Int = 0

  private def ch =
    if (readPosition < input.length) Some(input(readPosition).toString)
    else None
  def nextToken(): Token = {
    skipWhiteSpace()

    ch match {
      case Some("=") =>
        readChar()
        Token(ASSIGN, "=")
      case Some(";") =>
        readChar()
        Token(SEMICOLON, ";")
      case Some("(") =>
        readChar()
        Token(LPAREN, "(")
      case Some(")") =>
        readChar()
        Token(RPAREN, ")")
      case Some(",") =>
        readChar()
        Token(COMMA, ",")
      case Some("+") =>
        readChar()
        Token(PLUS, "+")
      case Some("{") =>
        readChar()
        Token(LBRACE, "{")
      case Some("}") =>
        readChar()
        Token(RBRACE, "}")
      case Some(str) =>
        if (isLetter(str)) {
          val literal = readIdentifier()
          Token.fromLiteral(literal)
        } else if (isDigit(str)) {
          val literal = readNumber()
          Token(INT, literal)
        } else Token(ILLEGAL, str)
      case _ =>
        readChar()
        Token(EOF, "")
    }
  }
  private def readChar(): Unit = {
    position = readPosition
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

  private def readIdentifier(): String = {
    val currentPosition = readPosition
    while (ch.exists(isLetter)) readChar()
    input.substring(currentPosition, readPosition)
  }

  private def readNumber(): String = {
    val currentPosition = readPosition
    while (ch.exists(isDigit)) readChar()
    input.substring(currentPosition, readPosition)
  }

}

object Lexer {
  def from(input: String): Lexer = Lexer(input)
}
