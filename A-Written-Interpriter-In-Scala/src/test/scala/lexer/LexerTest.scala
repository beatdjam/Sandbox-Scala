package lexer

import org.scalatest.FunSpec
import org.scalatest.MustMatchers.convertToAnyMustWrapper
import token._

class LexerTest extends FunSpec {

  describe("LexerTest") {
    it("first case") {
      val input = "=+(){},;"
      val list = Seq(
        (ASSIGN, "="),
        (PLUS, "+"),
        (LPAREN, "("),
        (RPAREN, ")"),
        (LBRACE, "{"),
        (RBRACE, "}"),
        (COMMA, ","),
        (SEMICOLON, ";"),
        (EOF, "")
      )

      val lexer = Lexer.from(input)

      list.foreach { expected =>
        val tok = lexer.nextToken()
        tok.tokenType mustEqual expected._1
        tok.literal mustEqual expected._2
      }
    }
  }
}
