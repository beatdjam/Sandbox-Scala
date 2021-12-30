import java.io.{File, PrintWriter}
import java.util.Date

// 9 制御構造の抽象化
// 9.1 コード重複の削減
// 関数呼び出し時に引数として関数値を与えることで実行時の挙動を決めることができる関数を高階関数と呼ぶ。
object FileMatcher {
  private def filesHere = new File(".").listFiles
  // 高階関数を使わない場合の記述
  //  def filesEnding(query: String) =
  //    for (file <- filesHere; if file.getName.endsWith(query)) yield file
  //  def filesContaining(query: String) =
  //    for (file <- filesHere; if file.getName.contains(query)) yield file
  //  def filesRegex(query: String) =
  //    for (file <- filesHere; if file.getName.matches(query)) yield file
  // 高階関数を使う場合の記述
  def filesMatching(matcher: String => Boolean) =
    for (file <- filesHere; if matcher(file.getName)) yield file

  def filesEnding(query: String) = filesMatching(_.endsWith(query))
  def filesContaining(query: String) = filesMatching(_.contains(query))
  def filesRegex(query: String) = filesMatching(_.matches(query))
}

// _.endsWith(_) は _ に対応する引数を受け付けてendWithの結果を返す関数として解釈される
// arg1.endWith(arg2)のように読み替えられる
// 更なる簡略化として_.endsWith(query) : String => Booleanのようにして、matcherのみを引数とした関数に変更できる

// 9.2 クライアントコードの単純化
// 引数の条件に応じた値を走査するループ処理を言語のAPIとして公開することで、コードの重複が削減されている
def containsNeg(nums: Seq[Int]): Boolean = nums.exists(_ < 0)
def containsOdd(nums: Seq[Int]): Boolean = nums.exists(_ % 2 == 1)

// 9.3 カリー化
// 非カリー化関数
def plainOldSum(x: Int, y: Int) = x + y
def curriedSum(x: Int)(y: Int) = x + y
// => def first(x: Int) = (y: Int) => x + y と分解できる

// curriedSum(1)(2)とfirst(2)は実行結果が等価
curriedSum(1)(2)
val first = curriedSum(1)_
first(2)

// 9.4 新しい制御構造を作る

// リソースをオープン -> 操作 -> クローズする制御構造
// このように、制御構造を持つ関数の中でリソースを生成して引数の値に利用させるパターンを「ローンパターン」という
// ローンパターンはリソースの生成からクローズまでの責任を持ち、外側は責任を持つ必要がない
def withPrintWriter(file: File, op: PrintWriter => Unit): Unit = {
  val writer = new PrintWriter(file)
  try {
    op(writer)
  } finally {
    writer.close()
  }
}
// これで呼び出せる
// withPrintWriter(
//   new File("date.txt"),
//   writer => writer.println(new Date)
// )

// Scalaでは引数を1個渡すメソッドは中括弧で囲むことができる
// println { "Text" } とか
def withPrintWriter(file: File)(op: PrintWriter => Unit): Unit = {
  val writer = new PrintWriter(file)
  try {
    op(writer)
  } finally {
    writer.close()
  }
}

// カリー化で引数を分割することで第2引数が切り出せるため、より制御構造らしく扱うことができる
// withPrintWriter(new File("date.txt")) { writer =>
//   writer.println(new Date)
// }

// 9.5 名前渡しパラメータ
// 中括弧の中のコードに値を渡さないifやwhileに近い制御構造を作るための仕組みとしての名前渡しパラメーター
var assertionsEnabled = true
def myAssert(predicate: () => Boolean): Unit =
  if (assertionsEnabled && !predicate()) throw new AssertionError
// この定義だと呼び出し時に myAssert(() => 5 > 3) のように呼び出す必要がある

// 空パラメーターを省略して　=> にすることで名前渡しパラメーターにできる
def byNameAssert(predicate: => Boolean): Unit =
  if (assertionsEnabled && !predicate) throw new AssertionError
// byNameAssert(5 > 3) のように自然な形で呼び出せる

// 下記のように書くこともできるが、byNameAssertとは意味合いが変わる
def boolAssert(predicate: Boolean): Unit =
  if (assertionsEnabled && !predicate) throw new AssertionError

assertionsEnabled = false
// boolAssertは引数がBooleanなので、渡す前に評価される
// boolAssert(5 / 0 == 0) // 実行するとzero divideのエラーが出る

// byNameAssertは引数に渡した値を評価する関数値が渡されるので、副作用が起きない
byNameAssert(5 / 0 == 0) // エラーが出ない

// 9.6 まとめ