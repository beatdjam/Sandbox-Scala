import scala.annotation.tailrec
import scala.language.implicitConversions
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

// lazy
case class Circle(x: Int, y: Int, radius: Int) {
  lazy val area: Double = {
    println("面積を計算します")
    radius * radius * math.Pi
  }
}

val c = Circle(0, 0, 5)
c.area
c.area

// 暗黙の型変換
val implicitConversion: Unit = {
  // 既存の型を要求する箇所に任意の型が来た時に読み替える型変換
  // 現在は非推奨とされている
  implicit def intToBoolean(n: Int): Boolean = n != 0

  if (1) {
    println("1 is true")
  }

  // 既存の型に存在しないメソッドを拡張し、元の型に生えてるように見せかける型変換
  class IntExt(val self: Int) {
    def isPositive: Boolean = self > 0
  }

  implicit def intExt(self: Int): IntExt = new IntExt(self)

  // Intに生えてないメソッドを呼んだ時、暗黙の型変換で読み替えられた先のメソッドが呼び出せる
  println(1.isPositive)
}

// 型を拡張するときはimplicit classを使うのが今風らしい
val implicitClass: Unit = {
  // 暗黙クラス
  implicit class IntExt(val self: Int) {
    def isPositive: Boolean = self > 0
  }
  println(1.isPositive)
}

// 暗黙パラメータ
// Intが渡されなかった時に暗黙的に解決する
implicit val context: Int = 1
def printContext(implicit ctx: Int): Unit = {
  println(ctx)
}
printContext

// リストの音要素の値を合計して返す
// implicit parameterを使わない場合
def sumInt(list: List[Int]): Int = list.foldLeft(0) {
  (acc, x) => acc + x
}
sumInt(List(1, 2, 3, 4))

// 加算するときの振る舞いのtrait
trait Adder[T] {
  def zero: T

  def plus(x: T, y: T): T
}

// 加算の処理
def sum[T](list: List[T])(implicit adder: Adder[T]): T = {
  list.foldLeft(adder.zero) { (acc, x) => adder.plus(acc, x) }
}

implicit object IntAdder extends Adder[Int] {
  def zero: Int = 0

  def plus(x: Int, y: Int): Int = x + y
}

implicit object DoubleAdder extends Adder[Double] {
  def zero: Double = 0.0

  def plus(x: Double, y: Double): Double = x + y
}

// 明示的にAdderの実装を渡す場合
//sum(List(1, 2, 3, 4))(IntAdder)
//sum(List(1.5, 2.0, 3.2, 4.9))(DoubleAdder)

// implicit parameterで自動的に対応するAdderを呼び出す
sum(List(1, 2, 3, 4))
sum(List(1.5, 2.0, 3.2, 4.9))
