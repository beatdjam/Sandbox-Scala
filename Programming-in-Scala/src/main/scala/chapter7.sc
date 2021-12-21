import java.io.File
import scala.annotation.tailrec
import scala.io.StdIn.readLine
// 7 組み込みの制御構造
// Scala組み込みの制御構造はif、 while、 for、 try、 match、関数呼び出ししかない
// 関数リテラルの組み合わせによって言語機能を実現しているからである
// Scalaの制御構造はほとんどのものが値を返す。

// 7.1 if式
// ifは値を返し、if ~ elseブロックでStringが返るのでfileNameはStringで推論される
val args = Seq[String]()
val fileName = if (args.nonEmpty) args.head else "default.txt"

// 7.2 whileループ
// Scalaのwhile、do-whileの挙動は他の言語と大きく変わらず、Unitを返す
// 純粋関数言語では取り除かれることが多いwhileだが、whileのほうが適している場面もあるのでScalaにはある
// => 再帰の代わりにwhileで表現するなど
// あんまり利用を推奨されない

// whileで書いた場合
def gcdLoop(x: Long, y:Long): Long = {
  var a= x
  var b = y
  while( a!= 0) {
    val temp = a
    a = b%a
    b = temp
  }
  b
}

// 再帰で書いた場合
@tailrec
def gcd(x: Long, y: Long): Long = if(y == 0) x else gcd(y, x % y)

// do-while
//var line = ""
//do {
//  line = readLine()
//  println("Read: " + line)
//} while (line != "")

// 7.3 for式
// 7.3.1 コレクションの反復処理
// <-(ジェネレーター)でコレクションを変数に受けて利用する
for (num <- 1 to 4)
  println(num)

// 7.3.2 フィルタリング
// ジェネレーターで生成された値をif節でフィルタリングできる
// 複数のフィルタリングを同時に行うこともできる
val oneToTen = 1 to 10
for(num <- oneToTen if num % 2 == 0 if num != 10)
  println(num)

// 7.3.3 反復処理のネスト
for(
  num <- 1 to 10 if num % 2 == 0;
  result <- num to 10
) println(result)

// 下に書いたのと同じ
// ネストしたループをジェネレーターの中で処理できる
for(
  num <- 1 to 10 if num % 2 == 0;
  result <- num to 10
) println(result)

(1 to 10)
  .filter(_ % 2 == 0)
  .flatMap(_ to 10)
  .foreach(println)