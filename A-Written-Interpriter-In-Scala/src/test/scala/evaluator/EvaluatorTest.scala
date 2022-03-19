package evaluator

import lexer.Lexer
import org.scalatest.FunSpec
import parser.Parser
import `object`._
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
        ("if (1 < 2) { 10 } else { 20 }", 10)
      )

      list.foreach { case (input, expected) =>
        val evaluated = testEval(input)
        evaluated match {
          case Some(Integer(value)) =>
            value mustEqual expected
          case Some(Bool(value)) =>
            value mustEqual expected
          case Some(value) =>
            value mustEqual expected
          case None =>
            fail("invalid object")
        }
      }
    }
  }

  private def testEval(input: String) = {
    val lexer = Lexer.from(input)
    val parser = Parser.from(lexer)
    val program = parser.parseProgram()
    Evaluator.eval(program)
  }
}
