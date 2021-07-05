// コンパニオンオブジェクト
class Person private(name: String, age: Int)

object Person {
  def apply(name: String, age: Int): Person = new Person(name, age)
}

// 部分関数
val dbl: PartialFunction[Int, Int] = {
  case n if n >= 0 => n * 2
}

dbl.isDefinedAt(1) // true
dbl.isDefinedAt(0) // true
dbl.isDefinedAt(-1) // false
dbl.apply(1) // 2 dbl(1)と等価

Seq(-1, 0, 1, 2, 3).collect(dbl) // List(0, 2, 4, 6)

// デフォルト引数
def join(list: List[String], separator: String = ","): String = list.mkString(separator)
join(List("a", "b", "c"), ".")
join(List("a", "b", "c"))


// 名前付き引数
join(list = List("a", "b", "c"), separator = ".")

// 値クラス
case class Meter(value: Int) extends AnyVal

println(Meter(10))

// 値クラスと暗黙クラスの組み合わせ
implicit class StringExtention(val self: String) extends AnyVal {
  def display(): Unit = println(self)
}

"Hoge".display()

// 型メンバ
// 型に別名を付けられる
class TypeMembers {
  type T = String
  val t: T = "Foo"
}

// 同じ型のaliasなら代入できちゃうっぽい
type K = String
val k = new TypeMembers().t
println(k)

// typeの宣言時点では型を明示せず、利用時に決めることもできる
trait Stacks {
  type T

  case class NonEmptyStack(head: T, tail: Stack) extends Stack {
    def isEmpty: Boolean = false

    def top: T = head

    def pop: Stack = tail
  }

  case object EmptyStack extends Stack {
    def isEmpty: Boolean = true

    def top: T = ???

    def pop: Stack = ???
  }

  sealed abstract class Stack {
    def isEmpty: Boolean

    def top: T

    def pop: Stack

    def push(e: T): Stack = NonEmptyStack(e, this)
  }
}

class IntStacks extends Stacks {
  type T = Int // ここで初めてTが決まる
  val stack = EmptyStack.push(1).push(2).push(3)
  println(stack.top)
  println(stack)
}

new IntStacks

// 自分型アノテーション
trait ModuleA {
  def methodA(): Unit
}

trait ModuleB {
  self: ModuleA => // 別のクラスの処理に依存させる
  def methodB(): Unit = {
    methodA()
  }
}

class ClassC extends ModuleB with ModuleA { // 利用時にmix-inする
  def methodA(): Unit = {
    println("methodA")
  }
}

val c = new ClassC()
c.methodA()
c.methodB() // ModuleAのmethodA(実装はClassC)を呼び出している

// 複数の引数リスト
def each[T](list: List[T])(f: T => Unit): Unit = {
  list.foreach(f)
}
each(List(1, 2, 3)) { x => println(x) }
// 部分適用
val f = each(List(1, 2, 3)) _
f { x => println(x * 2) }

// η-expansion
// 下記3つは処理としては等価
// foreachにlistの値を処理する無名関数を渡している
List(1, 2, 3, 4, 5).foreach(x => println(x))
// printlnに「 _」を渡すとメソッドから関数に変換できる
List(1, 2, 3, 4, 5).foreach(println _)
// 「 _」を省略しても成り立つ式の場合はメソッド自体を渡しても同じように解釈される
List(1, 2, 3, 4, 5).foreach(println)

// 引数の名前渡し
// 型名の前に=>をつけると利用時に評価される
def if_[A](condition: Boolean)(thenClause: => A)(elseClause: => A): A = {
  if (condition) thenClause else elseClause
}

val n = 3
if_(n % 2 == 1) {
  println("奇数")
} {
  println("偶数")
}

// 抽出子
object Positive {
  def unapply(n: Int): Option[Int] = if (n > 0) Some(n) else None
}

1 match {
  case Positive(x) =>
    println(s"$x is Positive")
  case x =>
    println(s"$x is Negative")
}

-1 match {
  case Positive(x) =>
    println(s"$x is Positive")
  case x =>
    println(s"$x is Negative")
}

// implicitの探索範囲
trait Adder[T] {
  def zero: T

  def plus(x: T, y: T): T
}

object Adder {
  implicit object IntAdder extends Adder[Int] {
    def zero: Int = 0

    def plus(x: Int, y: Int): Int = x + y
  }

  implicit object StringAdder extends Adder[String] {
    def zero: String = ""

    def plus(x: String, y: String): String = x + y
  }
}

def sum[T](list: List[T])(implicit adder: Adder[T]): T = list.foldLeft(adder.zero) {
  (x, y) => adder.plus(x, y)
}

sum(List(1, 2, 3))
sum(List("A", "B", "C"))