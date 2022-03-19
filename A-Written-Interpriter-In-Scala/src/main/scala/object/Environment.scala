package `object`

import scala.collection.mutable

class Environment private () {
  private val store = mutable.Map.empty[String, Object]
  def get(name: String): Option[Object] = store.get(name)
  def set(name: String, value: Object): Unit = store.addOne(name, value)
}

object Environment {
  def newEnvironment = new Environment()
}
