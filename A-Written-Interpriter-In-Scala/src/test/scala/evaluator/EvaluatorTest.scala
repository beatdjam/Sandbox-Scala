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
        ("false", false)
      )

      list.foreach { case (input, expected) =>
        val evaluated = testEval(input)
        evaluated match {
          case Some(Integer(value)) =>
            value mustEqual expected
          case Some(Bool(value)) =>
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
