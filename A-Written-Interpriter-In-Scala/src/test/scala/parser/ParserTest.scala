package parser

import ast.{
  BooleanExpression,
  ExpressionStatement,
  Identifier,
  IfExpression,
  InfixExpression,
  IntegerLiteral,
  LetStatement,
  PrefixExpression,
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
          val expression = statement.get.expression.get.asInstanceOf[Identifier]
          expression.value mustEqual "foobar"
          expression.tokenLiteral() mustEqual "foobar"
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
          val expression =
            statement.get.expression.get.asInstanceOf[IntegerLiteral]
          expression.value mustEqual 5
          expression.tokenLiteral() mustEqual "5"
        case _ =>
          fail("invalid statement")
      }
    }

    it("prefix expression") {
      // input, operator, integerValue
      val list = Seq(("!5", "!", 5), ("!5", "!", 5))
      list.foreach { case (input, operator, integerValue) =>
        val lexer = Lexer.from(input)
        val parser = Parser.from(lexer)
        val program = parser.parseProgram()
        checkParserErrors(parser)

        program.statements.length mustEqual 1
        program.statements.foreach {
          case statement: Some[ExpressionStatement] =>
            val expression =
              statement.get.expression.get.asInstanceOf[PrefixExpression]
            expression.operator mustEqual operator

            val integerLiteral = expression.right.asInstanceOf[IntegerLiteral]
            integerLiteral.value mustEqual integerValue
            integerLiteral.tokenLiteral() mustEqual integerValue.toString
          case _ =>
            fail("invalid statement")
        }
      }
    }
    it("infix expression") {
      // input, operator, integerValue
      val list = Seq(
        ("5 + 5;", 5, "+", 5),
        ("5 - 5;", 5, "-", 5),
        ("5 * 5;", 5, "*", 5),
        ("5 / 5;", 5, "/", 5),
        ("5 > 5;", 5, ">", 5),
        ("5 < 5;", 5, "<", 5),
        ("5 == 5;", 5, "==", 5),
        ("5 != 5;", 5, "!=", 5)
      )
      list.foreach { case (input, leftValue, operator, rightValue) =>
        val lexer = Lexer.from(input)
        val parser = Parser.from(lexer)
        val program = parser.parseProgram()
        checkParserErrors(parser)

        program.statements.length mustEqual 1
        program.statements.foreach {
          case statement: Some[ExpressionStatement] =>
            val expression =
              statement.get.expression.get.asInstanceOf[InfixExpression]
            expression.operator mustEqual operator

            val left = expression.left.asInstanceOf[IntegerLiteral]
            left.value mustEqual leftValue
            left.tokenLiteral() mustEqual leftValue.toString

            val right = expression.right.asInstanceOf[IntegerLiteral]
            right.value mustEqual rightValue
            right.tokenLiteral() mustEqual rightValue.toString
          case _ =>
            fail("invalid statement")
        }
      }
    }

    it("boolean expression") {
      // input, operator, integerValue
      val list = Seq(("true;", true), ("false;", false))
      list.foreach { case (input, boolean) =>
        val lexer = Lexer.from(input)
        val parser = Parser.from(lexer)
        val program = parser.parseProgram()
        checkParserErrors(parser)

        program.statements.length mustEqual 1
        program.statements.foreach {
          case statement: Some[ExpressionStatement] =>
            val expression =
              statement.get.expression.get.asInstanceOf[BooleanExpression]
            expression.value mustEqual boolean
          case _ =>
            fail("invalid statement")
        }
      }
    }

    it("if expression") {
      val list = Seq(
        ("if (x < y) { x }", "(x < y)", "x", None),
        ("if (x < y) { x } else { y }", "(x < y)", "x", Some("y"))
      )
      list.foreach { case (input, condition, consequence, alternative) =>
        val lexer = Lexer.from(input)
        val parser = Parser.from(lexer)
        val program = parser.parseProgram()
        checkParserErrors(parser)

        program.statements.foreach {
          case statement: Some[ExpressionStatement] =>
            val expression =
              statement.get.expression.get.asInstanceOf[IfExpression]

            expression.condition.getString mustEqual condition
            expression.consequence.statements.length mustEqual 1
            expression.consequence.statements.head.get.getString mustEqual consequence
            expression.alternative.map(_.getString) mustEqual alternative

          case _ =>
            fail("invalid statement")
        }
      }
    }

    it("operator precedence") {
      val list = Seq(
        ("-a * b", "((-a) * b)"),
        ("!-a", "(!(-a))"),
        ("a + b + c", "((a + b) + c)"),
        ("a + b - c", "((a + b) - c)"),
        ("a * b * c", "((a * b) * c)"),
        ("a * b / c", "((a * b) / c)"),
        ("a + b / c", "(a + (b / c))"),
        ("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)"),
        ("3 + 4; -5 * 5", "(3 + 4)\n((-5) * 5)"),
        ("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))"),
        ("5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))"),
        (
          "3 + 4 * 5 == 3 * 1 + 4 * 5",
          "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))"
        ),
        ("true", "true"),
        ("false", "false"),
        ("3 > 5 == false", "((3 > 5) == false)"),
        ("3 < 5 == true", "((3 < 5) == true)"),
        ("1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)"),
        ("(5 + 5) * 2", "((5 + 5) * 2)"),
        ("2 / (5 + 5)", "(2 / (5 + 5))"),
        ("-(5 + 5)", "(-(5 + 5))"),
        ("(!(true == true))", "(!(true == true))")
      )
      list.foreach { case (input, expected) =>
        val lexer = Lexer.from(input)
        val parser = Parser.from(lexer)
        val program = parser.parseProgram()
        checkParserErrors(parser)
        program.getString mustEqual expected
      }
    }
  }

  describe("parse") {
    it("Program to Object") {
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

  }

  def checkParserErrors(parser: Parser): Unit = {
    if (parser.errors.nonEmpty) {
      parser.errors.foreach(println)
      fail(s"parser has ${parser.errors.length} errors")
    }
  }
}
