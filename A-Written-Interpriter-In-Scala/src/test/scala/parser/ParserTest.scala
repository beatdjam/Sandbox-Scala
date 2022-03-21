package parser

import ast.{
  BooleanExpression,
  CallExpression,
  ExpressionStatement,
  FunctionLiteral,
  Identifier,
  IfExpression,
  InfixExpression,
  IntegerLiteral,
  LetStatement,
  PrefixExpression,
  Program,
  ReturnStatement,
  StringLiteral
}
import lexer.Lexer
import org.scalatest.FunSpec
import org.scalatest.MustMatchers.convertToAnyMustWrapper
import token.{IDENT, LET, Token}

class ParserTest extends FunSpec {

  describe("statements") {
    it("let statements") {

      val list = Seq(
        ("let x = 5;", "x", "5"),
        ("let y = true;", "y", "true"),
        ("let foobar = y;", "foobar", "y")
      )

      list.foreach { case (input, identifier, value) =>
        val lexer = Lexer.from(input)
        val parser = Parser.from(lexer)
        val program = parser.parseProgram()
        checkParserErrors(parser)

        program.statements.length mustEqual 1
        program.statements.foreach {
          case statement: Some[LetStatement] =>
            statement.get.tokenLiteral() mustEqual "let"
            statement.get.name.value mustEqual identifier
            statement.get.value.get.tokenLiteral() mustEqual value
          case _ =>
            fail("invalid statement")
        }
      }
    }

    it("return statements") {
      val list = Seq(
        ("return 5;", "return"),
        ("return 10;", "return"),
        ("return 838383;", "return")
      )

      list.foreach { case (input, expect) =>
        val lexer = Lexer.from(input)
        val parser = Parser.from(lexer)
        val program = parser.parseProgram()
        checkParserErrors(parser)

        program.statements.length mustEqual 1
        program.statements.foreach {
          case statement: Some[ReturnStatement] =>
            statement.get.tokenLiteral() mustEqual expect
          case _ =>
            fail("invalid statement")
        }
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

    it("string expression") {
      val list = Seq(("\"hello world\"", "hello world"))
      list.foreach { case (input, string) =>
        val lexer = Lexer.from(input)
        val parser = Parser.from(lexer)
        val program = parser.parseProgram()
        checkParserErrors(parser)

        program.statements.length mustEqual 1
        program.statements.foreach {
          case statement: Some[ExpressionStatement] =>
            val expression =
              statement.get.expression.get.asInstanceOf[StringLiteral]
            expression.value mustEqual string
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
        program.statements.length mustBe 1
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

    it("function literal") {
      val list = Seq(
        ("fn(x, y) {x + y}", Seq("x", "y"), "(x + y)"),
        ("fn() {};", Nil, ""),
        ("fn(x) {};", Seq("x"), ""),
        ("fn(x, y, z) {};", Seq("x", "y", "z"), "")
      )
      list.foreach { case (input, parameters, body) =>
        val lexer = Lexer.from(input)
        val parser = Parser.from(lexer)
        val program = parser.parseProgram()
        checkParserErrors(parser)

        program.statements.length mustBe 1
        program.statements.foreach {
          case statement: Some[ExpressionStatement] =>
            val expression =
              statement.get.expression.get.asInstanceOf[FunctionLiteral]

            expression.parameters.map(_.getString) mustEqual parameters
            expression.body.getString mustEqual body

          case _ =>
            fail("invalid statement")
        }
      }
    }

    it("call expression") {
      val list = Seq(
        ("add(1, 2 * 3, 4 + 5)", "add", Seq("1", "(2 * 3)", "(4 + 5)"))
      )
      list.foreach { case (input, name, parameters) =>
        val lexer = Lexer.from(input)
        val parser = Parser.from(lexer)
        val program = parser.parseProgram()
        checkParserErrors(parser)

        program.statements.length mustBe 1
        program.statements.foreach {
          case statement: Some[ExpressionStatement] =>
            val expression =
              statement.get.expression.get.asInstanceOf[CallExpression]
            expression.function.tokenLiteral() mustEqual name
            expression.arguments
              .map(_.getString) mustEqual parameters
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
        ("(!(true == true))", "(!(true == true))"),
        ("a + add(b * c) + d", "((a + add((b * c))) + d)"),
        (
          "add(a, b, 1, 2 * 3, 4 + 5, add(6, (7 * 8)))",
          "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))"
        ),
        ("add(a + b + c * d / f + g)", "add((((a + b) + ((c * d) / f)) + g))")
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
