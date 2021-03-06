package `object`

import ast.{BlockStatement, Identifier}

sealed abstract class ObjectType(val objectType: String) {
  override def toString: String = objectType
}
case object STRING_OBJ extends ObjectType("STRING")
case object INTEGER_OBJ extends ObjectType("INTEGER")
case object BOOLEAN_OBJ extends ObjectType("BOOLEAN")
case object NULL_OBJ extends ObjectType("NULL")
case object RETURN_OBJ extends ObjectType("RETURN")
case object ERROR_OBJ extends ObjectType("ERROR")
case object FUNCTION_OBJ extends ObjectType("FUNCTION")
case object BUILTIN_OBJ extends ObjectType("BUILTIN")
case object ARRAY_OBJ extends ObjectType("ARRAY")
case object HASH_OBJ extends ObjectType("HASH")

trait Object {
  val objectType: ObjectType
  def inspect: String
}

trait Hashable {
  val value: Any
}

case class Str(value: String) extends Object with Hashable {
  val objectType: ObjectType = STRING_OBJ
  override def inspect: String = value
}

case class Integer(value: Int) extends Object with Hashable {
  val objectType: ObjectType = INTEGER_OBJ
  override def inspect: String = value.toString
}

case class Bool(value: Boolean) extends Object with Hashable {
  val objectType: ObjectType = BOOLEAN_OBJ
  override def inspect: String = value.toString
}

case class Null() extends Object {
  val objectType: ObjectType = NULL_OBJ
  override def inspect: String = "null"
}

case class Return(value: Object) extends Object {
  val objectType: ObjectType = RETURN_OBJ
  override def inspect: String = value.inspect
}

case class Error(message: String) extends Object {
  val objectType: ObjectType = ERROR_OBJ
  override def inspect: String = s"ERROR: $message"
}

case class Array(elements: Seq[Object]) extends Object {
  val objectType: ObjectType = ARRAY_OBJ
  override def inspect: String = s"[${elements.map(_.inspect).mkString(", ")}]"
}

case class Function(
    parameters: Seq[Identifier],
    body: BlockStatement,
    env: Environment
) extends Object {
  val objectType: ObjectType = FUNCTION_OBJ
  override def inspect: String =
    s"fn(${parameters.map(_.value).mkString(", ")}) {\n ${body.getString}\n}"
}

case class Builtin(fn: Seq[Object] => Option[Object]) extends Object {
  val objectType: ObjectType = BUILTIN_OBJ
  override def inspect: String = s"builtin function"
}

case class Hash(pairs: Map[Object, Object]) extends Object {
  val objectType: ObjectType = HASH_OBJ
  override def inspect: String =
    s"{${pairs.map { case (key, value) => s"${key.inspect}: ${value.inspect}" }.mkString(", ")}}"
}
