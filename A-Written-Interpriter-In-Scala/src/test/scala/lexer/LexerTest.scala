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
          |_!-/*5;
          |5 < 10 > 5;
          |
          |if (5 < 10) {
          | return true;
          |} else {
          | return false;
          |}
          |
          |10 == 10;
          |10 != 9;
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
        (IDENT, "_"),
        (BANG, "!"),
        (MINUS, "-"),
        (SLASH, "/"),
        (ASTERISK, "*"),
        (INT, "5"),
        (SEMICOLON, ";"),
        (INT, "5"),
        (GT, "<"),
        (INT, "10"),
        (LT, ">"),
        (INT, "5"),
        (SEMICOLON, ";"),
        (IF, "if"),
        (LPAREN, "("),
        (INT, "5"),
        (GT, "<"),
        (INT, "10"),
        (RPAREN, ")"),
        (LBRACE, "{"),
        (RETURN, "return"),
        (TRUE, "true"),
        (SEMICOLON, ";"),
        (RBRACE, "}"),
        (ELSE, "else"),
        (LBRACE, "{"),
        (RETURN, "return"),
        (FALSE, "false"),
        (SEMICOLON, ";"),
        (RBRACE, "}"),
        (INT, "10"),
        (EQ, "=="),
        (INT, "10"),
        (SEMICOLON, ";"),
        (INT, "10"),
        (NOT_EQ, "!="),
        (INT, "9"),
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
