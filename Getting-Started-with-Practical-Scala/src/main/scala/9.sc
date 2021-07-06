import scala.annotation.tailrec

// 副作用で返却値が変わる例
var base = 0
def addAndSquare(i: Int): Int = {
  base += i
  base * base
}

// 外にあるbaseの値が書き換わってるので結果も変わる
addAndSquare(1) + addAndSquare(2) // 10
addAndSquare(2) + addAndSquare(1) // 61

def sumAndSquare(base: Int, i: Int): Int = {
  val newValue = base + i
  newValue * newValue
}
// baseの値を外から渡すので合成の順番を変えても結果が変わらない = 参照透過性
sumAndSquare(0, 1) + sumAndSquare(0, 2)
sumAndSquare(0, 2) + sumAndSquare(0, 1)

// 手続き型のスタイル
def sumUpAndShow(start: Int, end: Int): Unit = {
  var current = start
  var total = 0
  while (current <= end) {
    total += current
    current += 1
  }
  println(total)
}

// 表示と計算の分離
def sumUp1(start: Int, end: Int): Int = {
  var current = start
  var total = 0
  while (current <= end) {
    total += current
    current += 1
  }
  total
}

// 再帰にすることで可変な変数をなくす
def sumUp2(start: Int, end: Int): Int = {
  @tailrec
  def doSumUp(current: Int, subtotal: Int): Int =
    if (current > end)
      subtotal
    else
      doSumUp(current + 1, subtotal + current)

  doSumUp(start, 0)
}

// そもそもコレクションに生えてるメソッドを使う
def sumUp3(start: Int, end: Int): Int = (start to end).sum