package example

class ApplySample {
  def apply(): Unit = println("インスタンスメソッドのapply")
}

object ApplySample {
  def apply(): Unit = println("クラスメソッドのapply")
}