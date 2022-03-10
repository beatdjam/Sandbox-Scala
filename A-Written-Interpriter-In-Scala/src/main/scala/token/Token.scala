package token

case class Token(tokenType: TokenType, literal: String)
object Token {
  def fromLiteral(literal: String): Token = {
    val tokenType = literal match {
      case "fn"     => FUNCTION
      case "let"    => LET
      case "true"   => TRUE
      case "false"  => FALSE
      case "if"     => IF
      case "else"   => ELSE
      case "return" => RETURN
      case _        => IDENT
    }
    Token(tokenType, literal)
  }
  def fromOperatorLiteral(literal: String): Token = {
    val tokenType = literal match {
      case "=" => ASSIGN
      case "+" => PLUS
      case "-" => MINUS
      case "!" => BANG
      case "/" => SLASH
      case "*" => ASTERISK
      case "<" => GT
      case ">" => LT
      case _   => throw new IllegalArgumentException("invalid operator")
    }
    Token(tokenType, literal)
  }
  def fromDelimiterLiteral(literal: String): Token = {
    val tokenType = literal match {
      case "," => COMMA
      case ";" => SEMICOLON
      case "(" => LPAREN
      case ")" => RPAREN
      case "{" => LBRACE
      case "}" => RBRACE
      case _   => throw new IllegalArgumentException("invalid operator")
    }
    Token(tokenType, literal)
  }
}

sealed abstract class TokenType(val token: String)
case object ILLEGAL extends TokenType("ILLEGAL")
case object EOF extends TokenType("EOF")
// 識別子 リテラル
case object IDENT extends TokenType("IDENT")
case object INT extends TokenType("INT")
// 演算子
case object ASSIGN extends TokenType("=")
case object PLUS extends TokenType("+")
case object MINUS extends TokenType("-")
case object BANG extends TokenType("!")
case object ASTERISK extends TokenType("*")
case object SLASH extends TokenType("/")

case object LT extends TokenType("<")
case object GT extends TokenType(">")

// デリミタ
case object COMMA extends TokenType(",")
case object SEMICOLON extends TokenType(";")
case object LPAREN extends TokenType("(")
case object RPAREN extends TokenType(")")
case object LBRACE extends TokenType("{")
case object RBRACE extends TokenType("}")
// キーワード
case object FUNCTION extends TokenType("FUNCTION")
case object LET extends TokenType("LET")
case object TRUE extends TokenType("TRUE")
case object FALSE extends TokenType("FALSE")
case object IF extends TokenType("IF")
case object ELSE extends TokenType("ELSE")
case object RETURN extends TokenType("RETURN")
