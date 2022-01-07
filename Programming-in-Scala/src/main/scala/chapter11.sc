// 11 Scalaのクラス階層
// Scalaでは全てのクラスがAnyを継承する
// Nothingは他のすべてのクラスのサブクラスになっている

// 11.1 Scalaのクラス階層
// AnyはAnyVal(値クラス)とAnyRef(参照クラス)を持っている
// 値クラスを継承するクラスは抽象クラスかつfinalなので、newでのインスタンス化ができない
// Unitはvoid型にほぼ対応している
// 値クラスの空間は互いにサブクラスにはなっていないが、暗黙の型変換が定義されている

// 11.2 プリミティブ型の実装方法
// Scalaの整数などは必要なときにはJavaのIntegerに透過的に変換される
// Javaの参照型の==は参照透過性を比較しているので等値かを判定するためにはequalsを使う必要があった
// Scalaは型の表現に左右されず、値が等しいかを判定する
// ScalaでJavaのように参照等価比較を行いたいときはeq, neを利用する必要がある
val x = "abcd".substring(2)
val y = "abcd".substring(2)
x == y // true
x eq y // false
x ne y // true

// 11.3 再開の２つの型
// Scalaの最下位にはscala.Nullとscala.Nothingがある
// Nullはnull参照の型で、すべての参照クラス(=AnyRefのサブクラス)のサブクラスで、値型とは互換性がない
// Nothingは他のすべての型のSubTypeであり、この型の値は存在しない(=異常終了したときにしか用いられない)
// Nothingがなぜ他のすべての型のSubTypeである必要があるのかというと下記のような関数を成り立たせるためである
def divide(x: Int, y: Int): Int = {
  if (y != 0) {
    x / y // Intを返す
  } else {
    sys.error("zero divide!") // Nothingを返すがIntのSubTypeなので成立する
  }
}
// throw new RuntimeException() などもNothingを返す

// 11.4 独自の値クラスの定義
// 組み込みの値クラスと同様に独自の値クラスを定義することができる
// 値クラスにできるクラスは、パラメーターが1つで、内容はdefだけでなければならない
class Dollars(val amount: Int) extends AnyVal {
  override def toString: String = s"$amount"
}
val money = new Dollars(1000000)
money.amount

// 11.4.1 同じ型の羅列を避ける
// 過度な再利用をせず、個々の概念ごとに新しいクラスを定義するべき

// Stringの羅列の表現力の低い記述
def title(text: String, anchor: String, style: String) : String =
  s"<a id='$anchor'><h1 class='$style'>$text</h1></a>"

// それぞれを値クラスで表現した記述
class Anchor(val value: String) extends AnyVal
class Style(val value: String) extends AnyVal
class Text(val value: String) extends AnyVal
class Html(val value: String) extends AnyVal

def title(text: Text, anchor: Anchor, style: Style) : Html =
  new Html(s"<a id='${anchor.value}'><h1 class='${style.value}'>${text.value}</h1></a>")

// 11.5 まとめ