package `object`

import scala.collection.mutable

class Environment private (
    private val outer: mutable.Map[String, Object] = mutable.Map.empty
) {
  private val store = outer.clone()
  def get(name: String): Option[Object] = store.get(name) match {
    case value @ Some(_) => value
    case None            => None
  }
  def set(name: String, value: Object): Unit = store.addOne(name, value)
}

object Environment {
  def newEnvironment = new Environment()
  def newEnclosedEnvironment(outer: Environment) = new Environment(outer.store)
}
