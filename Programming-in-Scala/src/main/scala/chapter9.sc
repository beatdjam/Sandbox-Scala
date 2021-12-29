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
