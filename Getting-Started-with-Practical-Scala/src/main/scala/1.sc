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


def isAlphanumeric(str: String): Boolean = str.matches("[a-zA-Z0-9\\s]+")
// 関数を変数に束縛するときは _ をつける
val isAlphanumericF = isAlphanumeric _
Seq("Scala", "2.12").filter(isAlphanumericF).foreach(x => println(x))

def matchOption(maybeVal: Option[Int]): Int = maybeVal match {
  case Some(num) if num < 0 => 0
  case Some(num) => num
  case None => 0
}

println(matchOption(Some(41)))
println(matchOption(Some(-1)))
println(matchOption(None))

println(Seq(1, 2, 3).map(_ * 2))
println(Seq(1, 2, 3).map { i =>
  println(i)
  i * 2
})

val flatMapResults: Seq[Int] = (1 to 3).flatMap { i =>
  (2 to 4).flatMap { j =>
    (3 to 5)
      .map(k => i * j * k)
      .filter(_ % 3 == 0)
  }
}
println(flatMapResults)

val forResults: Seq[Int] = for {
  i <- 1 to 3
  j <- 2 to 4
  k <- 3 to 5
  result = i * j * k if result % 3 == 0
} yield result
println(forResults)