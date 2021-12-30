// 10 合成と継承
// 10.1 2Dレイアウトライブラリー

// 10.2 抽象クラス
// abstractは実装を持たない抽象メンバーを持つ可能性をマークする
// 抽象クラスはインスタンスを作ることができない
// 抽象メソッドにはabstract修飾子をつける必要はない
//abstract class Element {
//  def contents: Array[String]
//}

// 10.3 パラメーターなしのメソッドの定義
// def width(): Intのような空括弧メソッド
// def width : Intのようなパラメーターなしのメソッド
// パラメーターがなく値の読み出しだけならパラメーターなしのメソッドが推奨されている
// IOがあったり、副作用があるものは空括弧メソッドにする

// def width: Int は val width: Intとして定義することもできる
// この場合はクラスの初期化時に計算され、都度算出されなくなる代わりに呼び出しが僅かに早く、必要とするメモリ容量が増える
// Kotlinのcomputed propertyと同じかな

// 空括弧メソッドとパラメーターなしメソッドは相互にオーバーライドできる
// Javaからの呼び出し時にも読み替えられる
abstract class Element {
  def contents: Array[String]
  def height: Int = contents.length
  def width: Int = if (height == 0) 0 else contents(0).length
}

// 10.4 クラスの拡張
// extendsには2つの効果がある
// スーパークラスのprivateメンバー以外を継承し、基底クラスのサブクラスになる
// サブクラスはスーパークラスとして振る舞うことができる

// extendsが省略されているとき、暗黙的にAnyRefを継承していることになる(JavaのObject型のようなもの)

// スーパークラスで実装済みのものはサブクラスでオーバーライドできる
// スーパークラスで定義済み抽象メンバーは実装(implement)する必要がある
class ArrayElement(conts: Array[String]) extends Element {
  def contents: Array[String] = conts
}

// 10.5 メソッドとフィールドのオーバーライド
// Scalaではフィールドがパラメーターなしメソッドをオーバーライドすることもできる
class ArrayElement(conts: Array[String]) extends Element {
  val contents: Array[String] = conts
}
// Scalaは値(フィールド、メソッド、パッケージ、シングルトンオブジェクト)と型(クラス、トレイト)の2種類の名前空間しか持たない
// そのため、フィールドとメソッドは同じ名称にできない

// 10.6 パラメーターフィールドの定義
// パラメーターにval, varと可視性修飾子をつけることで、パラメーターとフィールドを兼ねるパラメーターフィールドとして定義できる
class ArrayElement(val contents: Array[String]) extends Element

// 10.7 スーパーコンストラクターの呼び出し
// スーパークラス名の後ろに括弧で渡したい引数を渡すことでスーパーコンストラクターを呼び出すことができる
// これを利用すると、Arrayを要求するスーパークラスを文字列を渡すサブクラスで拡張することができる
class LineElement(s: String) extends ArrayElement(Array(s)) {
  override def width = s.length
  override def height = 1
}
