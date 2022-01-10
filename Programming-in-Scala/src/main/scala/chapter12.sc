import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

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

// 12.4 Orderedトレイト
// Scalaでは、一般的な大小比較に役立つOrderedトレイトを提供している
// compareメソッドを定義するだけですべての比較メソッドを持つクラスが作れる
class Rational(n: Int, d: Int) extends Ordered[Rational] {
  require(d != 0) // 分母が0は不正

  // 最大公約数
  private val g = gcd(n.abs, d.abs)

  val numer: Int = n / g
  val denom: Int = d / g

  def this(n: Int) = this(n, 1)

  def +(that: Rational): Rational = {
    new Rational(
      numer * that.denom + that.numer * denom,
      denom * that.denom
    )
  }
  def +(i : Int): Rational = new Rational(numer + i * denom, denom)

  def -(that: Rational): Rational = {
    new Rational(
      numer * that.denom - that.numer * denom,
      denom * that.denom
    )
  }
  def -(i : Int): Rational = new Rational(numer - i * denom, denom)

  def * (that: Rational): Rational = new Rational(numer * that.numer, denom * that.denom)
  def * (i: Int): Rational = new Rational(numer * i, denom)

  def / (that: Rational): Rational = new Rational(numer * that.denom, denom * that.numer)
  def / (i: Int): Rational = new Rational(numer, denom * i)

  override def toString: String = s"$n / $d"

  def lessThan(that: Rational) = numer * that.denom < that.numer * denom
  def max(that: Rational) = if (this.lessThan(that)) that else this

  @tailrec
  private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)

  // compareを定義する
  def compare(that: Rational) = (this.numer * that.denom) - (that.numer * this.denom)
}

// 12.5 トレイトによるクラスの積み上げ可能な変更
// トレイトはクラスのメソッドに変更を加えて、それを複数積み上げていくことができる
abstract class IntQueue {
  def get(): Int
  def put(x: Int): Unit
}

class BasicIntQueue extends IntQueue {
  private val buf = new ArrayBuffer[Int]
  def get() = buf.remove(0)
  def put(x: Int): Unit = buf += x
}

// traitでスーパークラスを宣言するとき、このtraitをmixinできるのは同じスーパークラスを持っている場合のみ
// abstractで呼び出されるsuperはこのtraitをmixinする前にmixinされたものに動的に束縛される
trait Doubling extends IntQueue {
  abstract override def put(x: Int): Unit = { super.put(2 * x) }
}

val queue: BasicIntQueue = new BasicIntQueue
queue.put(10)
queue.put(20)
queue.get()
queue.get()

class MyQueue extends BasicIntQueue with Doubling
val queue2: MyQueue = new MyQueue
queue2.put(10)
queue2.get()

// インスタンス生成時にもmixinできる
val queue3: BasicIntQueue with Doubling = new BasicIntQueue with Doubling
queue3.put(10)
queue3.get()


// 積み上げ可能なtrait
trait Incrementing extends IntQueue {
  abstract override def put(x: Int): Unit = super.put(x + 1)
}
trait Filtering extends IntQueue {
  abstract override def put(x: Int): Unit = if(x >= 0) super.put(x)
}

// Filteringのputから呼び出された(>=0の値のみ)Incrementingのputのみがputされている
val queue4 = new BasicIntQueue with Incrementing with Filtering
queue4.put(-1)
queue4.put(0)
queue4.put(1)
queue4.get()
queue4.get()

// Incrementingのputで+1されたあとも負数のものがputされる
val queue5 = new BasicIntQueue with Filtering with Incrementing
queue5.put(-2)
queue5.put(-1)
queue5.put(0)
queue5.put(1)
queue5.get()
queue5.get()
queue5.get()
