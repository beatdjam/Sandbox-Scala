class Person(name: String, age: Int, private val weight: Int) {
  private[this] val privVal = "コンパニオンオブジェクトから見えない変数"
}

object Person {
  def apply(name: String, age: Int, weight: Int) = new Person(name, age, weight)
  def printWeight(): Unit = {
    val taro = new Person("Taro", 20, 70)
    println(taro.weight)
  }
}

Person("taro", 18, 50)