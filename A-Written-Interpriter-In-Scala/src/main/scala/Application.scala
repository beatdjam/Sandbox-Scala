import `object`.Environment
import evaluator.Evaluator
import lexer.Lexer
import parser.Parser

import scala.io.StdIn.readLine

object Application {
  def main(args: Array[String]): Unit = {
    println("This is the Monkey programming language!")
    val env = Environment.newEnvironment
    while (true) {
      val input = readLine(">>")
      if (input == "") return

      val lexer = Lexer.from(input)
      val parser = Parser.from(lexer)
      val program = parser.parseProgram()
      if (parser.errors.nonEmpty) printParserErrors(parser.errors)
      else {
        val evaluated = Evaluator.eval(program, env)
        evaluated match {
          case Some(value) => println(value.inspect)
          case None        => ()
        }
      }
    }
  }

  private def printParserErrors(errors: Seq[String]): Unit = {
    val monkeyFace = """            __,__
                       |   .--.  .-"     "-.  .--.
                       |  / .. \/  .-. .-.  \/ .. \
                       | | |  '|  /   Y   \  |'  | |
                       | | \   \  \ 0 | 0 /  /   / |
                       |  \ '- ,\.-'''''''-./, -' /
                       |   ''-' /_   ^ ^   _\ '-''
                       |       |  \._   _./  |
                       |       \   \ '~' /   /
                       |        '._ '-=-' _.'
                       |           '-----'""".stripMargin

    println(monkeyFace)
    println("Woops! We ran into some monkey business here!")
    println(" parser errors:")
    errors.foreach(error => println("\t" + error))
  }
}
