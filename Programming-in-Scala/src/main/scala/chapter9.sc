import java.io.File

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
