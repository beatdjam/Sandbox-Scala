import Element.elem
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

// 10.8 override修飾子の使い方
// 親クラスの具象メンバーをoverrideするすべてのメンバーにこの修飾子を付けなければならない
// 親クラスの抽象メンバーを実装するときはつけてもつけなくても良い
// 基底クラスのメンバーに対してあとから同名メンバーを追加したときなどに想定外のoverrideが起きないかコンパイル時点で判断できる

// 10.9 多相性と動的束縛
// 多相性 = ポリモーフィズム

class UniformElement(
    ch: Char,
    override val width: Int,
    override val height: Int
) extends Element {
  private val line = ch.toString * width
  override def contents: Array[String] = Array.fill(height)(line)
}

val e1 : Element = new ArrayElement(Array("Hello", "World"))
val ae : ArrayElement = new ArrayElement(Array("Hello", "World"))
val e2: Element = ae
val e3: Element = new UniformElement('x', 2, 3)

// 呼び出される実装は変数や式の型ではなく実行時のオブジェクトの型になる
// これを動的束縛と呼ぶ
abstract class Sample {
  def demo(): Unit = println("Sample's implementation invoked")
}

class Sample1 extends Sample {
  override def demo(): Unit = println("Sample1's implementation invoked")
}

class Sample2 extends Sample

def invokeDemo(e: Sample): Unit = e.demo()

// 引数の型に関わらず、渡されたオブジェクトの型で実行されてることがわかる
invokeDemo(new Sample1) // Sample1's implementation invoked
invokeDemo(new Sample2) // Sample's implementation invoked

// 10.10 finalメンバーの実装
// final修飾子のついたメンバーはサブクラスでoverrideできない
// 同様にfinal修飾子のついたクラスはサブクラス化することができない

// 10.11 合成か継承か
// is-a関係の場合は継承を、コードの再利用が目的なら合成を選択するのがよい
// クライアントがスーパークラス型として利用したいかもそのサブクラスを継承するかの目安になる

// 10.12 above, beside, toStringの実装
// 関係ないけどメモ
// for式はKotlinのCollectionに対するメソッドチェーンの読み替えだと思えば良い気がしてきた
// 可読性を落とさず一つのブロックに入れながら、それぞれがシーケンシャルに処理される的な

// newでの生成からあとの章で作ったファクトリメソッドに書き換えてある
abstract class Element {
  def contents: Array[String]
  def height: Int = contents.length
  def width: Int = if (height == 0) 0 else contents(0).length
  def above(that: Element): Element = {
    require(this.width == that.width) // 簡単のためここでは異なるwidthは考慮しない
    Element.elem(this.contents ++ that.contents)
  }
  def beside(that: Element): Element = {
    require(this.height == that.height) // 簡単のためここでは異なるheightは考慮しない
    // 下のfor式は大体これと同じ
    // val contents = this.contents.zip(that.contents).map{case (line1, line2) => line1 + line2}
    val contents = for ((line1, line2) <- this.contents.zip(that.contents)) yield line1 + line2
    Element.elem(contents)
  }
  // 統一形式アクセスの作法で()をつけない
  // 純粋で副作用の無い関数のため
  override def toString = contents.mkString("/n")
}

// 10.13 ファクトリーオブジェクトの定義
// ファクトリメソッドでオブジェクトを生成することで個別の詳細を隠蔽することができる
// ファクトリはコンパニオンオブジェクトに置くのが素直
// 知識をこのコンパニオンオブジェクトに集約できるので、各実装をprivateにして完全に隠蔽もできる
object Element {
  private class ArrayElement(val contents: Array[String]) extends Element

  private class LineElement(s: String) extends ArrayElement(Array(s)) {
    override def width = s.length
    override def height = 1
  }

  private class UniformElement(ch: Char, override val width: Int, override val height: Int) extends Element {
    private val line = ch.toString * width
    override def contents: Array[String] = Array.fill(height)(line)
  }
  def elem(contents: Array[String]) = new ArrayElement(contents)
  def elem(chr: Char, width: Int, height: Int) = new UniformElement(chr, width, height)
  def elem(line: String) = new LineElement(line)
}

// 10.14 高さと幅を調整するheightenとwiden
abstract class Element {
  def contents: Array[String]
  def height: Int = contents.length
  def width: Int = if (height == 0) 0 else contents(0).length
  def above(that: Element): Element = {
    require(this.width == that.width) // 簡単のためここでは異なるwidthは考慮しない
    Element.elem(this.contents ++ that.contents)
  }
  def beside(that: Element): Element = {
    require(this.height == that.height) // 簡単のためここでは異なるheightは考慮しない
    val contents = for ((line1, line2) <- this.contents.zip(that.contents)) yield line1 + line2
    Element.elem(contents)
  }

  // 左右にpaddingする関数
  def widen(w: Int): Element = {
    if (w <= this.width) this
    else {
      val left = Element.elem(' ', (w - width) / 2, height)
      val right = Element.elem(' ', w - width - left.width, height)
      left.beside(this).beside(right)
    }
  }

  // 上下にpaddingする関数
  def heighten(h: Int): Element = {
    if (h <= this.height) this
    else {
      val top = Element.elem(' ', width, (h - height) / 2)
      val bottom = Element.elem(' ', width, h - height - top.height)
      top.above(this).above(bottom)
    }
  }

  override def toString = contents.mkString("/n")
}

// 10.15 レイアウトライブラリーの昨日をすべて試せるアプリケーション
object Spiral {
  val space = elem(" ")
  val corner = elem("+")

  def spiral(nEdges: Int, direction: Int): Element = {
    if (nEdges == 1) elem("+")
    else {
      val sp = spiral(nEdges - 1, (direction + 3) % 4)
      val verticalBar = elem('|', 1, sp.height)
      val horizontalBar = elem('-', sp.width, 1)
      if (direction == 0) {
        corner.beside(horizontalBar)
          .above(sp.beside(space))
      } else if (direction == 1) {
        sp.above(space)
          .beside(corner.above(verticalBar))
      } else if (direction == 2) {
        space.beside(sp).above(horizontalBar.beside(corner))
      } else verticalBar.above(corner).beside(space.above(sp))
    }
  }
  def main(args: Array[String]) = {
    val nSides = args(0).toInt
    println(spiral(nSides,0))
  }
}