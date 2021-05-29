import scala.collection.mutable
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
case class MyString(s : String) {
  def concat(that: MyString) : MyString = MyString(this.s + that.s)
}

println(MyString("hoge") concat MyString("taro"))