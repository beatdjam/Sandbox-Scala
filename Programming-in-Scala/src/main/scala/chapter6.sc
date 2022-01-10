import scala.annotation.tailrec
import scala.language.implicitConversions
// 6 関数型スタイルのオブジェクト

// 6.1 Rationalクラスの仕様
// ・有理数(分子と分母で表現される割合によって表現される数)をあらわす
// ・加減乗除などの分数の動作に含まれる演算を認める
// ・ミュータブルな常態を持たない

// 6.2 Rationalクラスの構築
// イミュータブルなオブジェクトのメリデメ
// メリット: 動作が予測しやすい、オブジェクトの受け渡しが安全、状態がかわらない・壊れない
// デメリット: オブジェクトの内部構造をコピーしなければならない場面がある

// class parameterがprimary constructorを形成する

// 6.3 toStringメソッドのオーバーライド

// 6.4 事前条件のチェック
// イミュータブルなオブジェクトは構築時にデータの有効性を保証しなければならない
// 構築時にrequireを作用させることで不正な値を防ぐ事ができる

// 6.5 フィールドの追加
// イミュータブルなオブジェクトは加算時に自身に値を加算するのではなく、加算した新しいオブジェクトを生成して返す
// 加算時に参照するためにはpublicなフィールドである必要がある

// ↓こうしないのはなんでだろう => あとでやるっぽい
//class Rational(val numer: Int,val denom: Int) {
//  require(denom != 0)
//  override def toString: String = s"$numer / $denom"
//  def add(that: Rational): Rational = {
//    new Rational(
//      numer * that.denom + that.numer * denom,
//      denom * that.denom
//    )
//  }
//}

// 6.6 自己参照
// thisキーワードで自身の参照を表せるが省略できる場面が多い

// 6.7 補助コンストラクター
// 基本コンストラクター以外のコンストラクターを補助コンストラクターと呼ぶ
// 補助コンストラクターは必ず他のコンストラクターの呼び出しで始まる必要がある
// def this(n: Int) = this(n, 1)

// 6.8 privateフィールドとメソッド

// 6.9 演算子の定義

// 6.10 Scalaの識別子
// ユーザープログラムに$の文字を使ってはいけない
// Scalaの定数はJavaのようにX_OFFSETのような命名ではなく、XOffsetのように先頭文字が大文字のキャメルケースが使われることが多い

// 6.11 メソッドの多重定義
// Javaのオーバーロードと同じ

// 6.12 暗黙の型変換
// 暗黙で呼び出される変換メソッドを定義することで、作成したクラスに対する演算を適用させることができる
// implicit conversionは参照できるスコープにある必要がある

// 6.13 演算子メソッドと暗黙の型変換
// 暗黙の型変換や演算子の定義はユーザーから実処理を隠蔽する。
// 簡潔さ=読みやすさで無いことに注意して利用する

// 6.14 まとめ
class Rational(n: Int, d: Int) {
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
}

implicit def intToRational(x: Int): Rational = new Rational(x)

new Rational(1, 2)
val r = new Rational(2, 3)
2 * r