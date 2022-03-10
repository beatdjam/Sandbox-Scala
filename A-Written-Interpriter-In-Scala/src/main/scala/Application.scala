import lexer.Lexer
import token.EOF

import scala.io.StdIn.readLine
import scala.util.control.Breaks.{break, breakable}

object Application {
  def main(args: Array[String]): Unit = {
    val prompt = ">>"
    println("This is the Monkey programming language!")
    while (true) {
      val input = readLine(prompt)
      if (input == "") return

      val lexer = Lexer.from(input)
      breakable {
        while (true) {
          val tok = lexer.nextToken()
          if (tok.tokenType == EOF) break
          println(tok)
        }
      }
    }
  }
}
