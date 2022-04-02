package lexer

import token.{EOF, EQ, ILLEGAL, INT, NOT_EQ, STRING, Token}

case class Lexer private (private var input: String) {
  private def currentCh = input.headOption.map(_.toString)
  private def peekCh = if (input.length > 1) Some(input(1).toString) else None

  private var tokens: Seq[Token] = {
    def getToken = {
      def isString(ch: String): Boolean = !ch.contains("\"")
      def isLetter(ch: String): Boolean = {
        val list = ('a' to 'z') ++ ('A' to 'Z')
        ch.forall(list.contains) || ch == "_"
      }
      def isDigit(ch: String): Boolean = {
        val list = '0' to '9'
        ch.forall(list.contains)
      }
      def isWhiteSpace(ch: String) = ch.exists {
        case ' ' | '\n' | '\t' | '\r' => true
        case _                        => false
      }

      def read(fn: String => Boolean): String = {
        val result = input.takeWhile(s => fn(s.toString)).mkString
        readChar(result.length)
        result
      }

      read(isWhiteSpace)

      currentCh
        .map {
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
        }
        .getOrElse(Token(EOF, ""))
    }

    input.map(_ => getToken)
  }

  def nextToken(): Token = {
    val result = tokens match {
      case head +: tail => tokens = tail; head;
      case _            => Token(EOF, "")
    }
    result
  }

  private def readChar(count: Int = 1): Unit = {
    input = input.substring(count)
  }

}

object Lexer {
  def from(input: String): Lexer = Lexer(input)
}
