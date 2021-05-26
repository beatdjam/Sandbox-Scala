import java.util.Date

lazy val lazyDate = {
  println("Initializing a date value...")
  // ブロックの最後の値で初期化される
  new Date()
}

// 初期化ブロックは初回のみ実行される
println(lazyDate)
// 2回目は遅延初期化された値のみ出力される
println(lazyDate)


def isAlphanumeric(str:String): Boolean = str.matches("[a-zA-Z0-9\\s]+")
// 関数を変数に束縛するときは _ をつける
val isAlphanumericF = isAlphanumeric _
Seq("Scala", "2.12").filter(isAlphanumericF).foreach(x => println(x))