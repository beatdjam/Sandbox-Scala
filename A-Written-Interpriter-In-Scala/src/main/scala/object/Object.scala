package `object`
sealed abstract class ObjectType(val objectType: String)
case object INTEGER_OBJ extends ObjectType("INTEGER")
case object BOOLEAN_OBJ extends ObjectType("BOOLEAN")
case object NULL_OBJ extends ObjectType("NULL")

trait Object {
  val objectType: ObjectType
  def inspect: String
}

case class Integer(value: Int) extends Object {
  val objectType: ObjectType = INTEGER_OBJ
  override def inspect: String = value.toString
}

case class Bool(value: Boolean) extends Object {
  val objectType: ObjectType = BOOLEAN_OBJ
  override def inspect: String = value.toString
}
case class Null() extends Object {
  val objectType: ObjectType = NULL_OBJ
  override def inspect: String = "null"
}
