import scalaz.Scalaz._
import scalaz.{Enum, Equal}
// Equal
// === =/= assert_===
// 標準のsyntaxは異なる型でも比較できてしまう
1 == "2"

// Scalazは異なる型を比較するとCEになる
// 1 === "2"
1 === 1
1 === 2

// =/=はNot Equal
1.some =/= 2.some

// Not Equalのとき、Runtime errorになる
// 1 assert_=== 2

// Order
// lt gt lte gte min max

// 標準のsyntaxは異なる型でも比較できてしまう
1 > 2.0

// Scalazは異なる型を比較するとCEになる
// 1 gt 2.0

// ?|?は比較結果を返す
1 ?|? 2 // LT

// Show
// Show型のインスタンスを文字列として表現する
3.show
3.shows
// showの結果をコンソール出力
"hoge".println

// Enum
// |-> -+- --- from fromStep pred predx succ succx |--> |-> |==> |=>
'a' to 'e'
'a' |-> 'e'
3 |=> 5
'B'.succ

// Bounded
implicitly[Enum[Char]].min
implicitly[Enum[Int]].max

// 信号の型クラス
case class TrafficLight(name: String)
val red = TrafficLight("red")
val yellow = TrafficLight("yellow")
val green = TrafficLight("green")
implicit val TrafficLightEqual: Equal[TrafficLight] = Equal.equal(_ == _)
red === yellow
red === red

// Yes と No の型クラス
// 型クラス
trait CanTruthy[A] { self =>
  def truthys(a: A): Boolean
}
object CanTruthy {
  def apply[A](implicit ev: CanTruthy[A]): CanTruthy[A] = ev
  def truthys[A](f: A => Boolean): CanTruthy[A] = new CanTruthy[A] {
    def truthys(a: A): Boolean = f(a)
  }
}

// Implicit Conversion
trait CanTruthyOps[A] {
  def self: A
  implicit def F: CanTruthy[A]
  final def truthy: Boolean = F.truthys(self)
}
implicit def toCanIsTruthyOps[A](
    v: A
)(implicit ev: CanTruthy[A]): CanTruthyOps[A] = new CanTruthyOps[A] {
  def self = v
  implicit def F: CanTruthy[A] = ev
}

// Intへの型クラスのインスタンス
implicit val intCanTruthy: CanTruthy[Int] = CanTruthy.truthys({
  case 0 => false
  case _ => true
})
10.truthy

// Listへの型クラスのインスタンス
implicit def listCanTruthy[A]: CanTruthy[List[A]] = CanTruthy.truthys({
  case Nil => false
  case _   => true
})
List("foo").truthy

// Nilへの型クラスのインスタンス
implicit val nilCanTruthy: CanTruthy[scala.collection.immutable.Nil.type] =
  CanTruthy.truthys(_ => false)
Nil.truthy

// Booleanへの型クラスのインスタンス
implicit val booleanCanTruthy: CanTruthy[Boolean] = CanTruthy.truthys(identity)

def truthyIf[A: CanTruthy, B, C](cond: A)(ifyes: => B)(ifno: => C) =
  if (cond.truthy) ifyes
  else ifno
