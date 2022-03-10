package parser

import ast.{Identifier, LetStatement, Program, ReturnStatement}
import lexer.Lexer
import org.scalatest.FunSpec
import org.scalatest.MustMatchers.convertToAnyMustWrapper
import token.{IDENT, LET, Token}

class ParserTest extends FunSpec {

  describe("ParserTest") {
    it("let statements") {
      val input =
        """
          |let x = 5;
          |let y = 10;
          |let foobar = 838383;
          |""".stripMargin

      val lexer = Lexer.from(input)
      val parser = Parser.from(lexer)
      val program = parser.parseProgram()
      checkParserErrors(parser)
      val expected = Seq("x", "y", "foobar")

      program.statements.length mustEqual 3
      program.statements.zipWithIndex.foreach {
        case (statement: Some[LetStatement], index: Int) =>
          statement.get.tokenLiteral() mustEqual "let"
          statement.get.name.value mustEqual expected(index)
          statement.get.name.tokenLiteral() mustEqual expected(index)
        case _ =>
          fail("invalid statement")
      }
    }
    it("return statements") {
      val input =
        """
          |return 5;
          |return 10;
          |return 838383;
          |""".stripMargin

      val lexer = Lexer.from(input)
      val parser = Parser.from(lexer)
      val program = parser.parseProgram()
      checkParserErrors(parser)

      program.statements.length mustEqual 3
      program.statements.zipWithIndex.foreach {
        case (statement: Some[ReturnStatement], index: Int) =>
          statement.get.tokenLiteral() mustEqual "return"
        case _ =>
          fail("invalid statement")
      }
    }
  }

  it("test string") {
    val program = Program(
      Seq(
        Some(
          LetStatement(
            Token(LET, "let"),
            Identifier(Token(IDENT, "myVar"), "myVar"),
            Some(Identifier(Token(IDENT, "anotherVar"), "anotherVar"))
          )
        )
      )
    )
    program.getString mustEqual "let myVar = anotherVar;"
  }

  def checkParserErrors(parser: Parser): Unit = {
    if (parser.errors.nonEmpty) {
      parser.errors.foreach(println)
      fail(s"parser has ${parser.errors.length}")
    }
  }
}
