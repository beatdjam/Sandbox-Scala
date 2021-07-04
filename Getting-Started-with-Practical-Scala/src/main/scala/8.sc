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