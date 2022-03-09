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
    it("second case") {
      val input =
        """
          |let five = 5;
          |let ten = 10;
          |
          |let add = fn(x, y) {
          | x + y;
          |};
          |
          |let result = add(five, ten);
          |""".stripMargin
      val list = Seq(
        (LET, "let"),
        (IDENT, "five"),
        (ASSIGN, "="),
        (INT, "5"),
        (SEMICOLON, ";"),
        (LET, "let"),
        (IDENT, "ten"),
        (ASSIGN, "="),
        (INT, "10"),
        (SEMICOLON, ";"),
        (LET, "let"),
        (IDENT, "add"),
        (ASSIGN, "="),
        (FUNCTION, "fn"),
        (LPAREN, "("),
        (IDENT, "x"),
        (COMMA, ","),
        (IDENT, "y"),
        (RPAREN, ")"),
        (LBRACE, "{"),
        (IDENT, "x"),
        (PLUS, "+"),
        (IDENT, "y"),
        (SEMICOLON, ";"),
        (RBRACE, "}"),
        (SEMICOLON, ";"),
        (LET, "let"),
        (IDENT, "result"),
        (ASSIGN, "="),
        (IDENT, "add"),
        (LPAREN, "("),
        (IDENT, "five"),
        (COMMA, ","),
        (IDENT, "ten"),
        (RPAREN, ")"),
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
