import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.io.{BufferedSource, Source}
import scala.util.{Failure, Success}

// Threadで実行する非同期処理
var i = 0
new Thread(() => {
  (1 to 100).foreach(_ => i += 1)
}).start()

new Thread(() => {
  (1 to 100).foreach(_ => i += 1)
}).start()

// 複数スレッドでの操作なので、このタイミングで200になっていることが確定しない
println(i)

// ロックによる排他制御
new Thread(() => {
  (1 to 100).foreach(_ => i.synchronized(i += 1))
})

println(i)

// httpを行う同期的な処理
object HttpTextClient {
  def get(url: String): BufferedSource = Source.fromURL(url)
}

// 既存の関数をくるむことでFutureにできる
val responseFuture: Future[BufferedSource]
= Future(HttpTextClient.get("https://scalamatsuri.org/"))

// FutureにあとからCallbackを仕込むことができる
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
  case Failure(throwable) => println("エラーが発生 " + throwable.toString)
}

// Futureの実行を待つ
//Await.result(responseFuture, Duration.Inf)
//Await.result(failedFuture, Duration.Inf)

// 既にある値をFutureでくるむ
val futureSuccessful = Future.successful(2)
val futureFailure = Future.failed(new Exception("sample"))
//Await.result(futureSuccessful, Duration.Inf)
//Await.result(futureFailure, Duration.Inf)

// Futureに生えているmapはFutureの値を変換する
Future(HttpTextClient.get("https://scalamatsuri.org/"))
  .map(s => try s.mkString finally s.close)
  .onComplete {
    case Success(body) => println(body)
    case Failure(throwable) => throwable.printStackTrace()
  }

// 非同期にGETしてレスポンスのbodyを
def getAsync(url: String): Future[String] = Future(HttpTextClient.get(url))
  .map(s => try s.mkString finally s.close)

// 引数に渡した文字列からURLを非同期に抽出する
def extractURLAsync(body: String): Future[collection.Seq[String]] = {
  val urlRegex = """https?://[\w.:$%?&()=+\-~]+""".r
  Future(urlRegex.findAllIn(body).toSeq)
}

// FutureのListを作って処理する
val urlsFuture: Future[collection.Seq[String]] =
  getAsync("https://scalamatsuri.org/").flatMap(extractURLAsync)

urlsFuture.onComplete {
  case Success(list) => list.foreach(println)
  case Failure(t) => t.printStackTrace()
}

val urlsInMatsuri = getAsync("https://scalamatsuri.org/").flatMap(extractURLAsync)
val urlsInOfficial = getAsync("https://scala-lang.org/").flatMap(extractURLAsync)
val urls = for {
  mUrls <- urlsInMatsuri
  oUrls <- urlsInOfficial
} yield mUrls.appendedAll(oUrls)

urls.onComplete {
  case Success(list) => list.mkString(", ")
  case Failure(t) => t.printStackTrace()
}

// andThenはブロック内の処理結果に関わらず、inputと同じfutureを出力する
// Futureの値を用いて副作用のある処理を行う場合などに使う？らしい
val andThen = getAsync("https://scalamatsuri.org/")
andThen
  .andThen {
    case Success(body) => println("body :" + body)
  }
  .flatMap(extractURLAsync)
  .onComplete {
    case Success(list) => list.foreach(println)
    case Failure(t) => t.printStackTrace()
  }
//Await.result(andThen, Duration.Inf)

// recoverはFuture.successの値としてかえる？
// recoverWithはリトライなどをして、success/failure、どちらの値も返りうる？
// 後で再確認したほうが良さそう
val failed = Future.failed(new Exception("sample"))
failed.recover {
  case _ => "Failed"
}
Await.result(failed, Duration.Inf)

// recoverWithは
val failed2 = Future.failed(new Exception("sample"))
failed2.recoverWith {
  case t => Future.failed(t)
}
Await.result(failed2, Duration.Inf)

