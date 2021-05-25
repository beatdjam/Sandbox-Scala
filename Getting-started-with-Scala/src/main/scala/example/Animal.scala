package example

sealed abstract class Animal(val cry: String)

object Animal {
  def checkAnimal(animal: Animal): Unit = animal match {
    case Cat | Dog => println(s"${animal}です。「${animal.cry}と鳴きます」")
    case _ => print("なにかです")
  }
}

case object Cat extends Animal("meow")

case object Dog extends Animal("bow wow")
