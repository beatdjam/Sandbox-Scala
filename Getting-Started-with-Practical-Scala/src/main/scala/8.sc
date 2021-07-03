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