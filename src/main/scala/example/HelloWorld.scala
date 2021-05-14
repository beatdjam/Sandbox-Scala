package example

import example.FizzBuzz.{fizzBuzz, fizzBuzzMatch, fizzBuzzRecursion}

import scala.annotation.tailrec
import scala.util.Random

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
    fizzBuzz(n) // 愚直に書いたもの
    fizzBuzzMatch(n) // match式で書いたもの
    fizzBuzzRecursion(n) // 再帰で書いたもの

    // tailrecの最適化確認
    println(gcd(12, 30))

    // example.Polygon
    val edges = List(3, 4, 5)
    //      val triangle = new example.Triangle(edges)
    val triangle = example.Polygon.fromEdges(edges)
    println(triangle.n)
    println(triangle.area)

    // traitを利用した例
    val triangle2 = new BlueFrostedTriangle(edges)
    println(triangle2.n)
    println(triangle2.area)
    triangle2.printColor()
    println(triangle2.alpha)

    // 利用側であとからtraitをmixinすることもできる
    val triangle3 = new Triangle(edges) with Blue with Frosted
    println(triangle3.n)
    println(triangle3.area)
    triangle3.printColor()
    println(triangle3.alpha)

    // Sealed ClassでEnumを表現した例
    val cat = Cat
    Animal.checkAnimal(cat)

    // Applyの振る舞い
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

    // タプル
    // _1、_2は専用のアクセサがある
    // 分解宣言ができる
    val tuple21 = (1, "Hello")
    println(tuple21._1)
    println(tuple21._2)
    val (one, two) = tuple21
    println(one)
    println(two)
  }

  // 末尾最適化で値を返す場合のサンプル
  // 最大公約数の取得
  @tailrec
  def gcd(a: Int, b: Int): Int = {
    if (a % b == 0) b else gcd(b, a % b)
  }
}

// applyを定義した場合のインスタンスに紐づくメソッドとクラスに紐づくメソッドのサンプル
class ApplySample {
  def apply(): Unit = println("インスタンスメソッドのapply")
}

object ApplySample {
  def apply(): Unit = println("クラスメソッドのapply")
}

// case classのサンプル
// コンストラクタ引数は引き継がれるが、内部の値はcopy時にcopy先のinitializeで再生成される
case class Foo(i: Int) {
  val randomValue: Int = new Random().nextInt()
}
