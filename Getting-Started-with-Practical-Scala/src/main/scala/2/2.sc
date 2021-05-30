import scala.annotation.tailrec
// 複数行リテラル
"""
This is
a multiline String
literal
"""

// 区切り文字 | を指定することで字下げを削除できる
// stripMarginに渡す引数で区切り文字を変えることができる
"""
  |This is
  |a multiline String
  |literal
""".stripMargin

// 型メンバを使ってタプルに別名をつける
type PointTuple = (Int, Int, Int)
val p: PointTuple = (60, 70, 80)
println(p)

// クラス
class Point(val x: Int, val y: Int) {
  def distance(that: Point): Int = {
    val xDiff = math.abs(this.x - that.x)
    val yDiff = math.abs(this.y - that.y)
    math.sqrt(xDiff * xDiff + yDiff * yDiff).toInt
  }

  // classに対するplusはinfixや演算子overrideではない？
  def +(that: Point): Point = new Point(x + that.x, y + that.y)
}

// クラスの継承
abstract class Shape {
  def draw(): Unit = println("不明な図形")
}

class Triangle extends Shape {
  override def draw(): Unit = println("三角形")
}

class Rectangle extends Shape {
  override def draw(): Unit = println("四角形")
}

class UnknownShape extends Shape

new Triangle().draw()
new Rectangle().draw()
new UnknownShape().draw()

// トレイト
trait Namable {
  val name: String

  def display(): Unit = println(name)
}

class Employee(val name: String) extends AnyRef with Namable

new Employee("太郎").display()
//
//trait Enumerable[A] {
//  def foreach[B](fun: A => B): Unit
//
//  final def map[B](f: A => B): List[B] = {
//    val members = mutable.Buffer.empty
//    foreach { m => members += f(m) }
//    members.toList
//  }
//
//  final def filter(p: A => Boolean): List[A] = {
//    val members = mutable.Buffer.empty[A]
//    foreach { m => if (p(m)) members += m }
//    members.toList
//  }
//
//  final def toList: List[A] = {
//    val members = mutable.Buffer.empty[A]
//    foreach { m => members += m }
//    members.toList
//  }
//}

// Any method with a single parameter can be used as an infix operator.
// https://docs.scala-lang.org/tour/operators.html
case class MyString(s: String) {
  def concat(that: MyString): MyString = MyString(this.s + that.s)
}

println(MyString("hoge") concat MyString("taro"))

// 下のPointクラスとそのコンパニオンオブジェクトと等価
case class Point(x: Int, y: Int)

//class Point(val x: Int, val y: Int) {
//  override def hashCode(): Int = x + y
//
//  override def equals(that: Any): Boolean = that match {
//    case that: Point => x == that.x && y == that.y
//    case _ => false
//  }
//
//  override def toString: String = s"Point($x,$y)"
//}
//
//object Point {
//  def apply(x: Int, y: Int): Point = new Point(x, y)
//}

// ケースクラスはMapのKeyにできる
val map = Map(Point(10, 10) -> 1, Point(20, 20) -> 2)
map(Point(10, 10))
map(Point(20, 20))

val p = Point(1, 2)
p match {
  case Point(x, y) =>
    println(x)
    println(y)
}

// パターンマッチ
def reverse[A](list: List[A], result: List[A]): List[A] = list match {
  case x :: xs => reverse(xs, x :: result)
  case Nil => result
}

reverse(List(1, 2, 3), Nil)

def last2[A](list: List[A]): A = list match {
  // (先頭の要素, 何らかの値, 範囲外) の時に先頭の要素を最後から2番目として返す
  // :: を2回適用することで、 先頭の要素 :: (残りの要素の先頭 :: Nil) のようなネストしたパターンになっている
  case x :: _ :: Nil => x
  case _ :: xs => last2(xs)
  case _ => sys.error("Invalid List.Should have 2 elements.")
}

// ローカルメソッド
def factorial(n: Int): Int = {
  @tailrec
  def f(m: Int, x: Int): Int = if (m == 0) x else f(m - 1, x * m)

  f(n, 1)
}

factorial(5)