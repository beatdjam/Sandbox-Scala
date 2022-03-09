package token

case class Token(tokenType: TokenType, literal: String)

sealed abstract class TokenType(val token: String)
case object ILLEGAL extends TokenType("ILLEGAL")
case object EOF extends TokenType("EOF")

// 識別子 リテラル
case object IDENT extends TokenType("IDENT")
case object INT extends TokenType("INT")

// 演算子
case object ASSIGN extends TokenType("=")
case object PLUS extends TokenType("+")

// デリミタ
case object COMMA extends TokenType(".")
case object SEMICOLON extends TokenType(";")

case object LPAREN extends TokenType("(")
case object RPAREN extends TokenType(")")
case object LBRACE extends TokenType("{")
case object RBRACE extends TokenType("}")

// キーワード
case object FUNCTION extends TokenType("FUNCTION")
case object LET extends TokenType("LET")
