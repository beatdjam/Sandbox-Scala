import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.io.{BufferedSource, Source}
import scala.util.{Failure, Success}

var i = 0
new Thread(() => {
  (1 to 100).foreach(_ => i += 1)
}).start()

new Thread(() => {
  (1 to 100).foreach(_ => i += 1)
}).start()

println(i) // 複数スレッドでの操作なので、このタイミングで200になっていることが確定しない

// ロックによる排他制御
new Thread(() => {
  (1 to 100).foreach(_ => i.synchronized(i += 1))
})

println(i)

object HttpTextClient {
  def get(url: String): BufferedSource = Source.fromURL(url)
}

//
val responseFuture: Future[BufferedSource]
= Future(HttpTextClient.get("https://scalamatsuri.org/"))

responseFuture.onComplete {
  case Success(body) =>
    println(body.mkString)
    body.close()
  case Failure(throwable) => println("エラーが発生" + throwable.toString)
}

val failedFuture = Future(HttpTextClient.get("hoge"))
failedFuture.onComplete {
  case Success(body) =>
    println(body.mkString)
    body.close()
  case Failure(throwable) => println("エラーが発生" + throwable.toString)
}
