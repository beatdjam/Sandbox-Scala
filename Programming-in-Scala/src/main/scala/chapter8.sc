import scala.io.Source
// 8 関数とクロージャー

// 8.1 メソッド
// メソッドとはオブジェクトのメンバー関数の事
object LongLines {
  def processFile(fileName: String, width: Int): Unit = {
    val source = Source.fromFile(fileName)
    for (line <- source.getLines()) processLine(fileName, width, line)
  }

  private def processLine(fileName: String, width: Int, line: String): Unit = {
    if (line.length > width)
      println(fileName + ": " + line.trim)
  }
}

// 8.2 ローカル変数
// 関数のスコープ内で関数を定義できる(= スコープ関数)
// ローカル変数は外側のパラメーターにアクセスできる
def processFile(fileName: String, width: Int): Unit = {
  def processLine(line: String): Unit = {
    if (line.length > width)
      println(fileName + ": " + line.trim)
  }
  val source = Source.fromFile(fileName)
  for (line <- source.getLines()) processLine(line)
}

// 8.3 一人前のオブジェクトとしての関数
// Scalaは関数がファーストクラスオブジェクトなので、値として扱うことができる
// 関数リテラルは実行時に関数値としてコンパイルされ、オブジェクトとして存在するものに変換される

// 値に格納した関数も()をつけて関数として呼び出すことができる
val increase = (x: Int) => x + 1
increase(0) // 1

// 中括弧でくくると複数行の文を含むことができる
val increase = (x: Int) => {
  x + 1
}

// foreachのように引数として関数を受け取りそれを実行するようなものがある
(1 to 5).foreach(increase)

// 8.4 関数リテラルの短縮形
val someNumbers = Seq(-11, -10, -5, 0, 5, 10)
someNumbers.filter((x: Int) => x > 0)
// xの型がIntであることはsomeNumbersがSeq[Int]であることから自明なので省略できる
// 型が推論できるパラメータは括弧も省略できる
// => ターゲットによる型付け
someNumbers.filter(x => x > 0)

// 8.5 プレースホルダー構文
// 式の中で一度しか使われない「埋めるべき空白」は_で表現することができる
// _を書くとCollectionの各要素が割り当てられて処理される
someNumbers.filter(_ > 0)

// 下記は(Int, Int) => Intとして解釈される
// つまり、出現した_はそれぞれ別のパラメータとして認識されている
val f = (_: Int) + (_: Int)

// 8.6 部分適用された関数