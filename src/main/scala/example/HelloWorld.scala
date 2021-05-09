package example

import example.FizzBuzz.{fizzBuzz, fizzBuzzMatch, fizzBuzzRecursion}

import scala.annotation.tailrec

// Appトレイトでエントリポイントを設定したもの
object Hello extends App {
  println("example.Hello, World!")
}

// mainメソッドでエントリポイントを設定したもの
object ScalaTour {
  def main(args: Array[String]): Unit = {
    // HelloWorld
    val message = "example.Hello World"
    println(message)

    //     FizzBuzz
    val n = 15
    fizzBuzz(n)
    fizzBuzzMatch(n)
    fizzBuzzRecursion(n)

    // tailrec
    println(gcd(12, 30))

    // example.Polygon
    val edges = List(3, 4, 5)
    //      val triangle = new example.Triangle(edges)
    val triangle = example.Polygon.fromEdges(edges)
    println(triangle.area)

    // Animal
    val cat = Cat
    Animal.checkAnimal(cat)

    val sample = new ApplySample
    sample()
    ApplySample.apply()

    // Case Class
    // applyを呼び出してる
    val foo = Foo(10)
    println(foo.i)

    // case classにおいて下記は等価
    println(foo)
    println(foo.toString)

    // コンストラクタ引数のみ複製され、インスタンス内の変数はapplyで書き換わることを確認
    val foo2 = foo.copy()
    println(s"${foo.i}, ${foo.randomValue}")
    println(s"${foo2.i}, ${foo2.randomValue}")
  }

  @tailrec
  def gcd(a: Int, b: Int): Int = {
    if (a % b == 0) b else gcd(b, a % b)
  }
}