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
