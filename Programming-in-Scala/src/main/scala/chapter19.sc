// 19 型パラメーターの役割
// 19.1 関数型キュー
// 関数型のキューはミュータブルなキュートは異なり、その要素を含んだ新しいキューを返す

// 効率の良くない単純なキューの例
// enqueueはこの実行方法の場合、Listの要素数に比例する実行時間がかかる
class SlowAppendQueue[T](elems: List[T]) {
  def head = elems.head
  def tail = new SlowAppendQueue(elems.tail)
  def enqueue(x: T) = new SlowAppendQueue(elems ::: List(x))
}

// headとtailの効率が悪くなる
class SlowHeadQueue[T](smele: List[T]) {
  def head = smele.last
  def tail = new SlowHeadQueue[T](smele.init)
  def enqueue(x: T) = new SlowHeadQueue[T](x :: smele)
}

// 前後それぞれから並べた要素を持つことで解決できる
// enqueueのときは逆順のリストの先頭に追加し続け、取り出すタイミングで反転させてそのheadやtailを利用する
class Queue[T](private val leading: List[T], private val trailing: List[T]) {
  private def mirror =
    if (leading.isEmpty) new Queue(trailing.reverse, Nil) else this

  // 前から並べたリストの先頭を取得
  def head = mirror.leading.head
  // 前から並べたリストの先頭要素を取り除いたものと、逆順のリストから新しいQueueを生成
  def tail = {
    val q = mirror
    new Queue(q.leading.tail, q.trailing)
  }

  // 前から並べたリストと、先頭に新しい要素を追加した逆順のリストで新しいQueueを作成
  def enqueue(x: T) = new Queue(leading, x :: trailing)
}

val q = new Queue(List("one", "two", "three"), Nil)
q.enqueue("four").head
q.enqueue("four").tail.head

// 19.2 情報隠蔽
// 19.2.1 privateコンストラクターとファクトリメソッド
class Queue[T] private (private val leading: List[T], private val trailing: List[T])
// T*は連続パラメーターを表す記法
object Queue {
  def apply[T](xs: T*) = new Queue[T](xs.toList, Nil)
}

// 19.2.2 privateクラスというもう一つの方法
trait Queue[T] {
  def head: T
  def tail: Queue[T]
  def enqueue(x: T): Queue[T]
}
object Queue {
  def apply[T](xs: T*): Queue[T] = new QueueImpl[T](xs.toList, Nil)
  private class QueueImpl[T](private val leading: List[T], private val trailing: List[T]) extends Queue[T] {
    private def mirror =
      if (leading.isEmpty) new QueueImpl(trailing.reverse, Nil) else this

    // 前から並べたリストの先頭を取得
    def head = mirror.leading.head
    // 前から並べたリストの先頭要素を取り除いたものと、逆順のリストから新しいQueueを生成
    def tail = {
      val q = mirror
      new QueueImpl(q.leading.tail, q.trailing)
    }

    // 前から並べたリストと、先頭に新しい要素を追加した逆順のリストで新しいQueueを作成
    def enqueue(x: T) = new QueueImpl(leading, x :: trailing)
  }
}

// 19.3 変位指定アノテーション
// Queueはtraitなので型として扱えないが、パラメータ化された型を定義できる
// このような振る舞いをするものを方コンストラクターとも呼ぶ
// def doesNotCompile(q: Queue)
def doesCompile(q: Queue[AnyRef]): Unit

// 型パラメータは基本的に非変なので、Queue[String]とQueue[AnyRef]はサブ型の関係にない
// traitをQueue[+T]のように記述することで共変にできる
// traitをQueue[-T]のように記述することで反変にできる

// 19.3.1 変位指定と配列
// Javaは歴史的経緯によりArrayが共変だが、Scalaは非変になっている

// 19.4 変位指定アノテーションのチェック
// 再代入のフィールドに対する共変など、ランタイムエラーが起きうる変位指定はコンパイラによってチェックされる
// Queue[+T]のような宣言をしたとき、Queue[Any] = Queue[Int]が成り立ってしまう。
// Queue[Any].enqueueにはQueue[String]をenqueueできてしまい、エラーが起きる可能性がある
// こういった実装はコンパイルに失敗する

// 19.5 下限境界
// TをUの下限境界にしているので、UはTのsuper typeである必要がある
// Tが下限境界になることで、Tを共変にしたときに起きていた問題が解決でき、下記のような記述ができる
trait Queue[+T] {
  def head: T
  def tail: Queue[T]
  def enqueue[U >: T](x: U): Queue[U]
}

// 19.6 反変
// AnyRefをwriteできる場合Stringもwriteできる
trait OutputChannel[-T] {
  def write(x: T): Unit
}

// S => Tの関数を書いたとき下記のようなtraitに読み替えられる
trait Function1[-S, +T] {
  def apply(x:S): T
}

// 例
// Library.printBookListはBook => AnyRefだけど、
// CustomerではPublication => Stringとして使ってる
// StringはAnyRefのサブ型、PublicationはBookのスーパー型なのでこれらは許容される
// Function1[-S, +T]なのでこれらが成り立つ
class Publication(val title: String)
class Book(title: String) extends Publication(title)

object Library {
  val books: Set[Book] = Set(
    new Book("A"),
    new Book("B")
  )

  def printBookList(info: Book => AnyRef): Unit = {
    for (book <- books) println(info(book))
  }
}

object Customer extends App {
  def getTitle(p: Publication): String = p.title
  Library.printBookList(getTitle)
}

// 非変
class Hoge[T]
val hoge: Hoge[String] = new Hoge[String]
// 共変
class Hoge[+T]
val hoge: Hoge[AnyRef] = new Hoge[String]
// 反変
class Hoge[-T]
val hoge: Hoge[String] = new Hoge[AnyRef]

// 19.7 object-privateデータ
// object-privateでの宣言によって、+Tの共変の宣言が可能になっている
// leading, trailingがvarになり、再代入で反転を表現することで、n回繰り返してもleadingが枯渇しない
// そのため、trailing.reverseが実行されることによるパフォーマンス劣化も起きない
class Queue[+T](private[this] var leading: List[T], private[this] var trailing: List[T]) {
  private def mirror(): Unit = if (leading.isEmpty) {
    while (trailing.nonEmpty) {
      leading = trailing.head :: leading
      trailing = trailing.tail
    }
  }

  def head = {
    mirror()
    leading.head
  }

  def tail: Queue[T] = {
    mirror()
    new Queue(leading.tail, trailing)
  }

  def enqueue[U >: T](x: U) = new Queue[U](leading, x :: trailing)
}