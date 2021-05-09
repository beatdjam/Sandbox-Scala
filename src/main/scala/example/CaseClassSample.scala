package example

import scala.util.Random

case class Foo(i: Int) {
  val randomValue = new Random().nextInt()
}