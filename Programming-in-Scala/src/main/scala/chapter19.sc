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
