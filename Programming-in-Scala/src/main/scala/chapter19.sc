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
// TODO あとでもう一度読む
class Queue[T](private val leading: List[T], private val trailing: List[T]) {
  private def mirror =
    if (leading.isEmpty) new Queue(trailing.reverse, Nil) else this

  def head = mirror.leading.head
  def tail = {
    val q = mirror
    new Queue(q.leading.tail, q.trailing)
  }

  def enqueue(x: T) = new Queue(leading, x :: trailing)
}
