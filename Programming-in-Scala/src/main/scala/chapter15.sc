import Element.elem

import scala.math.Pi

// 15 ケースクラスとパターンマッチ
// 15.1 単純な例
abstract class Expr
case class Var(name: String) extends Expr // 変数
case class Number(num: Double) extends Expr // 数値
case class UnOp(operator: String, arg: Expr) extends Expr // 単項
case class BinOp(operator: String, left: Expr, right: Expr) extends Expr // 二項

// 15.1.1 ケースクラス
// caseという修飾子をつけたクラスをcase classと呼ぶ
// この修飾子のついたクラスは暗黙的にいくつかの実装を追加される

// 1. 暗黙的にfactoryが定義される
val value = Var("x") // newがいらない

// 2. パラメータに暗黙的なvalプレフィックスが追加される
value.name

// 3. toString, hashCode, equalsの実装が追加される
// クラスとそのすべての引数の木構造を文字列化、ハッシュ化、比較する
// ==はequalsを利用するので、case classの場合は実際の構造とその値に基づいて比較される

// value.toString
// value.hashCode()
// value.equals()

// 4. copyメソッドが追加される
// 一部のパラメータが異なるインスタンスを作るときに、元のインスタンスから一部の値を置き換えたインスタンスを生成できる
val op = BinOp("+", Number(1), value)
op.copy(operator = "-")

// 15.1.2 パターンマッチ
// match式はパターンと1つ以上の式が含まれており、マッチしたパターンに対応する式が評価される
// 定数とマッチさせるパターン、eのように変数をマッチさせるパターンがある
// 変数はマッチした後、評価される式の中で値として使うことができる
def simplifyTop(expr: Expr): Expr = expr match {
  case UnOp("-", UnOp("-", e)) => e // 負の負は負のまま
  case BinOp("+", e, Number(0)) => e // 0の加算は元のまま
  case BinOp("*", e, Number(1)) => e // 1の乗算は元のまま
  case _ => expr
}

// 15.1.3 matchとswitchの違い
// Javaスタイルのswitchを一般化したもの
// フォールスルーはなく、マッチする式がなければエラーになる

// 15.2 パターンの種類

// 15.2.1 ワイルドカードパターン
// ワイルドカードパターン(_)はあらゆる値にマッチする
// パターンに含めない無視する値に対しても利用する

// 15.2.2 定数パターン
def describe(x: Any) = x match {
  case 5 => "five"
  case true => "truth"
  case "hello" => "hi!"
  case Nil => "the empty list"
  case _ => "something else"
}

// 15.2.3 変数パターン
def describe(x: Any) = x match {
  case 0 => "five"
  case something => "something:" + something.toString
}

// 先頭が小文字になっているとパターン変数、大文字だと定数とみなす
math.E match {
  case math.Pi => "Pi = " + Pi // Piにはmatchしない
  case pi => "not pi = " + pi
  case _ => "something" // piにmatchするので到達しない
}

// 15.2.4 コンストラクターパターン
// コンストラクタに渡される値がマッチするかどうかをmatchする
// 入れ子になっていてもパターンを検査する
def simplifyTop(expr: Expr): Expr = expr match {
  case BinOp("+", e, Number(0)) => e
}

// 下記3つのマッチングを一気に行っている
// BinOpかを確認
// 第3引数がNumberかを確認
// Numberの値が0かを確認

// 15.2.5 シーケンスパターン
// 長さが3で先頭が0
Seq(0, 1, 2) match {
  case List(0, _, _) => println("found")
  case _ => println("not found")
}

// 長さが1以上で先頭が0
Seq(0, 1, 2, 3) match {
  case List(0, _*) => println("found")
  case _ => println("not found")
}

// 15.2.6 タプルパターン
(0, 1, 2, 3) match {
  case (one, two, three, four) => println(one, two, three, four)
  case _ => println("not found")
}

// 15.2.7 型付きパターン
// 型付きパターンにmatchした値はその型として扱うことができる
"test" match {
  case s: String => println(s)
  case _ => println("something")
}

2 match {
  case i: Int => println(i)
  case _ => println("something")
}

// 15.2.7.1 型消去
// JavaとScalaは型引数の情報をコンパイル時に消去する
// そのため、Map[String, String]とMap[Int, Int]を区別できない
// Arrayは特別扱いをされているので、要素型をmatchに利用することができる

// 15.2.8 変数束縛パターン
// 変数名 @ パターンの順序で書けば変数束縛パターンになる
// nにNumber(0)を束縛する
def simplifyTop(expr: Expr): Expr = expr match {
  case BinOp("+", _,n @ Number(0)) => n
}

// 15.3 パターンガード
def simplifyAdd(e: Expr) = e match {
  case BinOp("+", x, y) if x == y => BinOp("*", x, Number(2))
  case  _ => e
}

// 15.4 パターンのオーバーラップ
// パターンは上からマッチングされる
// 上段で広い範囲のマッチングを行うと到達しないケースができる可能性がある
// その場合コンパイラはwarningを出力する

// 15.5 シールドクラス
// Scalaには同じファイルで定義されたクラス以外をサブクラスにできないsealed classがある
// sealed classを継承したcase classをつかってmatch式を書くと、パターンが網羅されていない場合に
// warnが出る
sealed abstract class Expr
case class Var(name: String) extends Expr
case class Number(num: Double) extends Expr
case class UnOp(operator: String, arg: Expr) extends Expr
case class BinOp(operator: String, left: Expr, right: Expr) extends Expr
// warnが出るケース
def simplifyAdd(e: Expr) = e match {
  case BinOp("+", x, y) if x == y => BinOp("*", x, Number(2))
}

// warnを出さないケース
def simplifyAdd(e: Expr) = (e: @unchecked) match {
  case BinOp("+", x, y) if x == y => BinOp("*", x, Number(2))
}

// 15.6 Option型
// 存在するかわからない値のためにOption型がある
// 存在すればSome(x), 存在しなければNoneになる
// Option型はmatch式で分解できる
(Some(1) : Option[Int]) match {
  case Some(x) => x
  case None => -1
}

// 15.7 パターンの意外な用途

// 15.7.1 変数定義におけるパターン
// valやvarの定義ではパターンを使うことができる
val tuple = (123, "abc")
val (num, str) = tuple

// case classでパターンにマッチングさせた変数を宣言することもできる
val exp = BinOp("*", Number(5), Number(1))
val BinOp(op, left, right) = exp

// 15.7.2 部分関数としてのケースシーケンス
// 中括弧で囲んだ選択肢のシーケンスは関数リテラルとして利用できる

//{
//  case Some(x) => x
//  case None => -1
//}

// ケースシーケンスは部分関数になる

// 空リストなどを渡すと失敗する関数
val second: List[Int] => Int = {
  case _ :: y :: _ => y
}
//second(Nil) // MatchError

// 部分関数が定義されているかをチェックするには、部分関数型を使って書く必要がある
val second: PartialFunction[List[Int], Int] = {
  case _ :: y :: _ => y
}
second.isDefinedAt(List()) // true
second.isDefinedAt(List(5, 6, 7)) // false

// PartialFunctionで宣言されたものはisDefinedAtでチェックできるようになる

// 15.7.3 for式内のパターン
// パターンはfor式の中でも使える
val results = List(Some("apple"), None, Some("orange"))
for (Some(fruit) <- results) println(fruit) // パターンにmatchしない値は捨てられる

// 15.8 これまでよりも大規模なコード例
// 数式フォーマッターを作る
sealed abstract class Expr
case class Var(name: String) extends Expr
case class Number(num: Double) extends Expr
case class UnOp(operator: String, arg: Expr) extends Expr
case class BinOp(operator: String, left: Expr, right: Expr) extends Expr

// 過去に作ったElementクラス
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
  def elem(contents: Array[String]): Element = new ArrayElement(contents)
  def elem(chr: Char, width: Int, height: Int): Element = new UniformElement(chr, width, height)
  def elem(line: String): Element = new LineElement(line)
}

abstract class Element {
  def contents: Array[String]
  def height: Int = contents.length
  def width: Int = if (height == 0) 0 else contents(0).length
  def above(that: Element): Element = {
    val this1 = this widen that.width
    val that1 = that widen this.width
    Element.elem(this1.contents ++ that1.contents)
  }
  def beside(that: Element): Element = {
    val this1 = this heighten that.height
    val that1 = that heighten this.height
    val contents = for ((line1, line2) <- this1.contents.zip(that1.contents)) yield line1 + line2
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

  override def toString = contents.mkString("\n")
}

class ExprFormatter {
  // 演算子の優先順位の昇順
  private val opGroups = Seq(
    Set("|", "||"),
    Set("&", "&&"),
    Set("^"),
    Set("==", "!="),
    Set("<", "<=", ">", ">="),
    Set("+", "-"),
    Set("*", "%")
  )

  // 優先順位を生成する
  private val precedence = {
    val assocs = for {
      i <- opGroups.indices
      op <- opGroups(i)
    } yield op -> i
    assocs.toMap
  }

  // 単項演算子の優先順位はどの演算子よりも高い
  private val unaryPrecedence = opGroups.length
  // 除算演算子の優先順位は-1
  private val fractionPrecedence = -1

  // 与えられたExprの種類によってフォーマット処理を分ける
  private def format(e: Expr, enclPrec: Int): Element = {
    e match {
      case Var(name) => elem(name)

      case Number(num) =>
        def stripDot(s: String) = {
          if (s endsWith ".0") s.substring(0, s.length - 2)
          else s
        }
        elem(stripDot(num.toString))

      case UnOp(op, arg) =>
        elem(op) beside format(arg, unaryPrecedence)

      case BinOp("/", left, right) =>
        val top = format(left, fractionPrecedence)
        val bot = format(right, fractionPrecedence)
        val line = elem('-', top.width max bot.width, 1)
        val frac = top above line above bot
        if (enclPrec != fractionPrecedence) frac
        else elem(" ") beside frac beside elem(" ")

      case BinOp(operator, left, right) =>
        val opPrec = precedence(op)
        val l = format(left, opPrec)
        val r = format(right, opPrec + 1)
        val oper = l beside elem(" " + op + " ") beside r
        if (enclPrec <= opPrec) oper
        else elem("(") beside oper beside elem(")")
    }
  }
  def format(e: Expr): Element = format(e, 0)
}