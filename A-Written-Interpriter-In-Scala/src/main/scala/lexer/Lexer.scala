package lexer

import token.{
  ASSIGN,
  COMMA,
  EOF,
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
    if (readPosition < input.length) input(readPosition).toString else ""
  def nextToken(): Token = {
    val token = ch match {
      case "=" => Token(ASSIGN, "=")
      case ";" => Token(SEMICOLON, ";")
      case "(" => Token(LPAREN, "(")
      case ")" => Token(RPAREN, ")")
      case "," => Token(COMMA, ",")
      case "+" => Token(PLUS, "+")
      case "{" => Token(LBRACE, "{")
      case "}" => Token(RBRACE, "}")
      case _   => Token(EOF, "")
    }
    readChar()
    token
  }
  private def readChar(): Unit = {
    position = readPosition
    readPosition = readPosition + 1
  }
}
object Lexer {
  def from(input: String): Lexer = Lexer(input)
}
