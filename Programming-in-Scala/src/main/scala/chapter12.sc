// 12 トレイト
// 12.1 トレイトの仕組み
// traitキーワードを利用して定義する
// 明示しなければAnyRefがスーパークラスになる
// トレイトは具象・抽象メソッドを持つことができる
trait Philosophical {
  def philosophize(): Unit = {
    println("I consume memory. therefore I am!")
  }
}

// 定義したtraitはextendでmixin(合成)できる
class Frog extends Philosophical {
  override def toString: String = "green"
}
new Frog().philosophize() // I consume memory. therefore I am!

// 複数traitを利用する場合はwithを利用する
// class Frog extends Animal with Philosophical with HasLegs

// traitの実装をオーバーライドすることができる
class Frog extends Philosophical {
  override def philosophize(): Unit = println("I am frog.")
}

// traitはJavaのinterfaceに具象メソッドを持てるようになったものに近いが、より拡張されている

// classと違って
// trait NoPoint(x:Int, y:Int)
// のようにクラスパラメーターを利用することはできない

// traitのsuper呼び出しは、実際にmixinされたときに決定する

// 12.2 シンインターフェイスとリッチインターフェイス
// 貧弱なシンインターフェイス
// 豊かな立地インターフェイス

// シンインターフェイスの例
trait CharSequence {
  def charAt(index: Int): Char
  def length: Int
  def subSequence(start: Int, end: Int): CharSequence
  def toString(): String
}

// Scalaのtraitは具象メソッドを持てるので、リッチインターフェイスを提供しやすい

// 12.3 具体例: 矩形オブジェクト
class Point(val x:Int, val y:Int)
trait Rectangular {
  def topLeft: Point
  def bottomRight: Point

  def left = topLeft.x
  def right = bottomRight.x
  def width = right - left
}

class Rectangle(val topLeft: Point, val bottomRight: Point) extends Rectangular
