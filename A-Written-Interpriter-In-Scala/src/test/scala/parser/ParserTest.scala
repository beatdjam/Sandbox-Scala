package parser

import ast.{
  ExpressionStatement,
  Identifier,
  IntegerLiteral,
  LetStatement,
  Program,
  ReturnStatement
}
import lexer.Lexer
import org.scalatest.FunSpec
import org.scalatest.MustMatchers.convertToAnyMustWrapper
import token.{IDENT, LET, Token}

class ParserTest extends FunSpec {

  describe("statements") {
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
      program.statements.foreach {
        case statement: Some[ReturnStatement] =>
          statement.get.tokenLiteral() mustEqual "return"
        case _ =>
          fail("invalid statement")
      }
    }
  }

  describe("expression") {
    it("identifier expression") {
      val input =
        """
          |foobar;
          |""".stripMargin

      val lexer = Lexer.from(input)
      val parser = Parser.from(lexer)
      val program = parser.parseProgram()
      checkParserErrors(parser)

      program.statements.length mustEqual 1
      program.statements.foreach {
        case statement: Some[ExpressionStatement] =>
          statement.get.tokenLiteral() mustEqual "foobar"
          val ident = statement.get.expression.get.asInstanceOf[Identifier]
          ident.value mustEqual "foobar"
          ident.tokenLiteral() mustEqual "foobar"
        case _ =>
          fail("invalid statement")
      }
    }
    it("digit expression") {
      val input =
        """
          |5;
          |""".stripMargin

      val lexer = Lexer.from(input)
      val parser = Parser.from(lexer)
      val program = parser.parseProgram()
      checkParserErrors(parser)

      program.statements.length mustEqual 1
      program.statements.foreach {
        case statement: Some[ExpressionStatement] =>
          val ident = statement.get.expression.get.asInstanceOf[IntegerLiteral]
          ident.value mustEqual 5
          ident.tokenLiteral() mustEqual "5"
        case _ =>
          fail("invalid statement")
      }
    }
  }

  describe("test string") {
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
