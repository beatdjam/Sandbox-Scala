package example

import example.FizzBuzz.{fizzBuzz, fizzBuzzMatch, fizzBuzzRecursion, toFizzBuzz}

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

    val fizzBuzzList = toFizzBuzz((1 to 15).toList)
    fizzBuzzList.foreach(x => println(x))

    // tailrecの最適化確認
    println(gcd(12, 30))

    // example.Polygon
    val edges = List(3, 4, 5)
    //      val triangle = new example.Triangle(edges)
    val polygon = example.Polygon.fromEdges(edges)
    polygon match {
      case Some(triangle) =>
        println(triangle.n)
        println(triangle.area)
      case None => println("未実装の図形です")
    }

    // traitを利用した例
    val triangle2 = new BlueFrostedTriangle(edges)
    println(triangle2.n)
    println(triangle2.area)
    triangle2.printColor()
    println(triangle2.alpha)

    // 利用側であとからtraitをmixinすることもできる
    //    val triangle3 = new Triangle(edges) with Blue with Frosted
    //    println(triangle3.n)
    //    println(triangle3.area)
    //    triangle3.printColor()
    //    println(triangle3.alpha)
    val polygon2 = Polygon.fromEdges(edges)
    println(polygon2)
    polygon2 match {
      case Some(polygon) => println(polygon.area)
      case None => println("不正な値です")
    }
    val invalidPolygon = Polygon.fromEdges(List(3, 4, 100))
    invalidPolygon match {
      case Some(polygon) => println(polygon.area)
      case None => println("不正な値です")
    }

    val invalidPolygon2 = Polygon.fromEdges(List(3, 4))
    invalidPolygon2 match {
      case Some(polygon) => println(polygon.area)
      case None => println("不正な値です")
    }

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

    val intBox = new Box[Int](10)
    println(intBox.element)
    val animalBox = new Box[Animal](cat)
    println(animalBox.element)
    animalBox.set(Dog)
    println(animalBox.element)

    val animalBox2 = new AnimalBox[Animal](Dog)
    println(animalBox2.element)
    val animalBox3 = new CatBox[Cat.type](cat)
    println(animalBox3.element)

    // Nil.::(1).::(2).::(3) と等価
    val list = 1 :: 2 :: 3 :: Nil
    println(list) // => List(1, 2, 3)
    println(list.head)
    println(list.tail)
    println(list.tail.head)
    println(list.tail.tail)

    val list2 = 0 :: list
    println(list2) // => List(0, 1, 2, 3)
    println(threeTimesThree((0 to 10).toList))
  }


  // 末尾最適化で値を返す場合のサンプル
  // 最大公約数の取得
  @tailrec
  def gcd(a: Int, b: Int): Int = {
    if (a % b == 0) b else gcd(b, a % b)
  }

  // Listの要素一つずつにアクセスし、3の倍数だけ3倍したリストを生成する
  def threeTimesThree(list: List[Int]): List[Int] = list match {
    // tailが存在してheadが3で割り切れる場合にheadを3倍したものを先頭要素にして残りの要素を再帰で取得
    case head :: tail if head % 3 == 0 => (head * 3) :: threeTimesThree(tail)
    // tailが存在する場合にheadを先頭要素にして残りの要素を再帰で取得
    case head :: tail => head :: threeTimesThree(tail)
    // tailがNilで渡された場合はNilを返す
    case Nil => Nil
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

// パラメータ多相のサンプル
class Box[T](var element: T) {
  def get(): T = element

  def set(newElement: T): Unit = {
    element = newElement
  }
}

// TをAnimalの派生クラスに制限した場合
class AnimalBox[T <: Animal](var element: T) {
  def get(): T = element

  def set(newElement: T): Unit = {
    element = newElement
  }
}

// Tを自身か親クラスに制限する場合
// これだと例がよくないかも…
class CatBox[T >: Cat.type](var element: T) {
  def get(): T = element

  def set(newElement: T): Unit = {
    element = newElement
  }
}