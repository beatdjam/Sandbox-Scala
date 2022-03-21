package `object`

import scala.collection.mutable

class Environment private (private val outer: Option[Environment] = None) {
  private val store = mutable.Map.empty[String, Object]
  def get(name: String): Option[Object] = store.get(name) match {
    case value @ Some(_) => value
    case None            => outer.flatMap(_.get(name))
  }
  def set(name: String, value: Object): Unit = store.addOne(name, value)
}

object Environment {
  def newEnvironment = new Environment()
  def newEnclosedEnvironment(outer: Environment) = new Environment(Some(outer))
}
