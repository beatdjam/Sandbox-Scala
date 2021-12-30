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
abstract class Element {
  def contents: Array[String]
  def height: Int = contents.length
  def width: Int = if (height == 0) 0 else contents(0).length
}
