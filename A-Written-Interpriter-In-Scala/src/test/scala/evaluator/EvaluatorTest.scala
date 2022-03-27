package evaluator

import lexer.Lexer
import org.scalatest.FunSpec
import parser.Parser
import `object`._
import ast.ArrayLiteral
import org.scalatest.MustMatchers.convertToAnyMustWrapper

class EvaluatorTest extends FunSpec {
  describe("EvaluatorTest") {

    it("input eval") {
      val list = Seq(
        ("5", 5),
        ("10", 10),
        ("true", true),
        ("false", false),
        ("!true", false),
        ("!false", true),
        ("!5", false),
        ("!!true", true),
        ("!!false", false),
        ("!!5", true),
        ("-5", -5),
        ("-10", -10),
        ("5 + 5 + 5 + 5 - 10", 10),
        ("2 * 2 * 2 * 2 * 2", 32),
        ("-50 + 100 -50", 0),
        ("5 * 2 + 10", 20),
        ("5 + 2 * 10", 25),
        ("20 + 2 * -10", 0),
        ("50 / 2 * 2 + 10", 60),
        ("2 * (5 + 10)", 30),
        ("3 * 3 * 3 + 10", 37),
        ("3 * (3 * 3) + 10", 37),
        ("(5 + 10 * 2 + 15 / 3) * 2 + -10", 50),
        ("1 < 2", true),
        ("1 > 2", false),
        ("1 < 1", false),
        ("1 > 1", false),
        ("1 == 1", true),
        ("1 != 1", false),
        ("1 == 2", false),
        ("1 != 2", true),
        ("true == true", true),
        ("false == false", true),
        ("true == false", false),
        ("true != false", true),
        ("false != true", true),
        ("(1 < 2) == true", true),
        ("(1 < 2) == false", false),
        ("(1 > 2) == true", false),
        ("(1 > 2) == false", true),
        ("if (true) { 10 }", 10),
        ("if (false) { 10 }", Null()),
        ("if (1) { 10 }", 10),
        ("if (1 < 2) { 10 }", 10),
        ("if (1 > 2) { 10 }", Null()),
        ("if (1 > 2) { 10 } else { 20 }", 20),
        ("if (1 < 2) { 10 } else { 20 }", 10),
        ("return 10;", 10),
        ("return 10; 9;", 10),
        ("return 2 * 5; 9;", 10),
        ("return 10;", 10),
        ("9; return 2 * 5; 9;", 10),
        (
          """
            | if (10 > 1) {
            |   if (10 > 1) {
            |     return 10;
            |   }
            |   return 1; 
            |}
            |""".stripMargin,
          10
        ),
        ("let a = 5; a;", 5),
        ("let a = 5 * 5; a;", 25),
        ("let a = 5; let b = a; b;", 5),
        ("let a = 5; let b = a; let c = a + b + 5; c;", 15),
        ("let identity = fn(x) { x; }; identity(5);", 5),
        ("let identity = fn(x) { return x; }; identity(5);", 5),
        ("let double = fn(x) { x * 2; }; double(5);", 10),
        ("let add = fn(x, y) { x + y; }; add(5, 5);", 10),
        ("let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));", 20),
        ("fn(x) { x; }(5)", 5),
        (
          """
            |let newAdder = fn(x) { 
            |   fn(y) { x + y }; 
            |};
            |let addTwo = newAdder(2);
            |addTwo(2);
            |""".stripMargin,
          4
        ),
        ("\"Hello World!\"", "Hello World!"),
        ("\"Hello\" + \" \" + \"World!\"", "Hello World!"),
        ("len(\"\")", 0),
        ("len(\"four\")", 4),
        ("len(\"hello world\")", 11),
        ("len(1)", "len: argument to len not supported, got INTEGER"),
        (
          "len(\"one\",\"two\")",
          "len: wrong number of arguments. got=2, want=1"
        ),
        ("[1, 2, 3][0]", 1),
        ("[1, 2, 3][1]", 2),
        ("[1, 2, 3][2]", 3),
        ("let i = 0; [1][i];", 1),
        ("[1, 2, 3][1 + 1];", 3),
        ("let myArray = [1, 2, 3];myArray[2];", 3),
        ("let myArray = [1, 2, 3];myArray[0] + myArray[1] + myArray[2];", 6),
        ("let myArray = [1, 2, 3];let i = myArray[0];myArray[i];", 2),
        ("[1, 2, 3][3]", Null()),
        ("[1, 2, 3][-1]", Null())
      )

      list.foreach { case (input, expected) =>
        val evaluated = testEval(input)
        evaluated match {
          case Some(Integer(value)) =>
            value mustEqual expected
          case Some(Bool(value)) =>
            value mustEqual expected
          case Some(Str(value)) =>
            value mustEqual expected
          case Some(Error(message)) =>
            message mustEqual expected
          case Some(value) =>
            value mustEqual expected
          case None =>
            fail(s"invalid object. $input $evaluated")
        }
      }
    }

    it("builtin function") {
      val list = Seq(
        ("len(\"\")", 0),
        ("len(\"four\")", 4),
        ("len(\"hello world\")", 11),
        ("len(1)", "len: argument to len not supported, got INTEGER"),
        (
          "len(\"one\",\"two\")",
          "len: wrong number of arguments. got=2, want=1"
        ),
        ("len([1, 2, 3])", 3),
        ("len([])", 0),
        ("len([1 + 1, \"test\", if (10 > 9) { 5 } else { 6 }])", 3),
        ("first([1, 2, 3])", 1),
        ("first([])", Null()),
        ("first(\"one\")", "o"),
        ("first(\"\")", Null()),
        ("last([1, 2, 3])", 3),
        ("last([])", Null()),
        ("last(\"one\")", "e"),
        ("last(\"\")", Null()),
        ("rest([1, 2, 3])", Seq("2", "3")),
        ("rest([])", Nil),
        ("rest(\"one\")", "ne"),
        ("rest(\"\")", ""),
        ("push([1, 2, 3], 4)", Seq("1", "2", "3", "4")),
        ("push([], 0)", Seq("0"))
      )

      list.foreach { case (input, expected) =>
        val evaluated = testEval(input)
        evaluated match {
          case Some(Integer(value)) =>
            value mustEqual expected
          case Some(Str(value)) =>
            value mustEqual expected
          case Some(Array(elements)) =>
            elements.map(_.inspect) mustEqual expected
          case Some(Error(message)) =>
            message mustEqual expected
          case Some(value) =>
            value mustEqual expected
          case _ | None =>
            fail(s"invalid object. $input $evaluated")
        }
      }
    }
    it("test function") {
      val list = Seq(
        ("fn(x) { x + 2; };", Seq("x"), "(x + 2)")
      )
      list.foreach { case (input, expectedParameter, expectedBody) =>
        val evaluated = testEval(input)
        evaluated match {
          case Some(Function(parameters, body, _)) =>
            parameters.map(_.getString) mustBe expectedParameter
            body.getString mustBe expectedBody
          case None => fail(s"invalid object. $input $evaluated")
        }
      }
    }
    it("array test") {
      val list = Seq(
        ("[1, 2 * 2, 3 + 3]", Seq("1", "4", "6"))
      )
      list.foreach { case (input, expected) =>
        val evaluated = testEval(input)
        evaluated match {
          case Some(Array(elements)) =>
            elements.map(_.inspect) mustEqual expected
          case None => fail(s"invalid object. $input $evaluated")
        }
      }
    }

    it("error handling") {
      val list = Seq(
        ("5 + true", "type mismatch: INTEGER + BOOLEAN"),
        ("5 + true; 5;", "type mismatch: INTEGER + BOOLEAN"),
        ("-true", "unknown operator: -BOOLEAN"),
        ("true + false;", "unknown operator: BOOLEAN + BOOLEAN"),
        ("5; true + false; 5", "unknown operator: BOOLEAN + BOOLEAN"),
        (
          "if (10 > 1) { true + false; }",
          "unknown operator: BOOLEAN + BOOLEAN"
        ),
        (
          """
            | if (10 > 1) {
            |   if (10 > 1) {
            |     return true + false;
            |   }
            |   return 1; 
            |}
            |""".stripMargin,
          "unknown operator: BOOLEAN + BOOLEAN"
        ),
        ("foobar", "identifier not found: foobar")
      )

      list.foreach { case (input, expected) =>
        val evaluated = testEval(input)
        evaluated match {
          case Some(Error(message)) =>
            message mustEqual expected
          case _ =>
            fail(s"invalid object. $input $evaluated")
        }
      }
    }
  }

  private def testEval(input: String) = {
    val lexer = Lexer.from(input)
    val parser = Parser.from(lexer)
    val program = parser.parseProgram()
    val env = Environment.newEnvironment
    Evaluator.eval(program, env)
  }
}
