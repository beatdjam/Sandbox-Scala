// 16 リストの操作

// 16.1 リストリテラル
// リストと配列の違い
// ・イミュータブルである
// ・再帰的な構造を持つ

// 16.2 List型
// Listの要素はすべて同じ型で、共変である
// => SがTのサブ型のとき、List[S]はList[T]のサブ型
// 空リストはNothingはあらゆる型のサブ型であることからList[Nothing]になる

// 16.3 リストの構築
Nil // 空リスト

// 下記は等価
List(1, 2, 3)
1 :: 2 :: 3 :: Nil // :が末尾につく演算子は右結合になるので、Nilから解決される

// 16.4 リストの基本演算
// head, tail, isEmpty
// head, tailが定義されるのは空でないリストにだけ
// headは先頭要素を、tailは先頭以外の要素のリストを返す

// 16.5 リストパターン
// リストはパターンマッチで分解できる
val a :: b :: c = List("a", "b", "c")
val List(a, b, c) = List("a", "b", "c")

// 要素数がわからないときは末尾に残りの要素がマッチする
val a :: rest = List("a", "b", "c")

// 16.6 Listクラスの一階メソッド

// 16.6.1 リストの連結
List(1, 2) ::: List(3, 4, 5)

// 16.6.2 分割等値原則
// Listの演算は扱いやすいケースに分割する「分割」と、algorithmを再帰的に呼び出して組み立てる「統治」で作られていることが多い

// Listは後ろから構築されるため、ysはそのままにしてxsについて考える
// 先頭の要素を取り出して残りの要素とysを再帰的に呼び出す
// こうすることで、xsのすべての要素を１つずつ結合して、最期にysを結合したListが出来上がる
def append[T](xs: List[T], ys:List[T]): List[T] = xs match {
  case List() => ys
  case x :: xs1 => x :: append(xs1, ys)
}

// 16.6.3 リストの長さを計算する: length
// リストの長さ計算は比較的コストが高いため、list.length == 0 とlist.isEmptyと等価ではない

// 16.6.4 リストの末尾へのアクセス: initとlast
// lastは最期の要素を、initは最後の要素を除いたListを返す
// initとlastはhead, tailと違い、リスト全体をたどって結果値を取得する必要がある

// 16.6.5 リストの反転
// リストの末尾に複数回アクセスが必要な場合、反転させてから先頭の要素にアクセスするほうがよい場合がある
// reverseはリストの長さに応じて計算時間がかかることに注意

// 16.6.6 プレフィックスとサフィックス: drop, take, splitAt
// dropとtakeはリストの先頭と末尾から任意の要素数のリストを取り出すもの
// splitAtは指定のindexでListを分割するもの

// 16.6.7 要素の選択: applyとindices
// 要素の選択はlist(n), list.apply(n)で表現されるが、nの大きさに比例する時間がかかるためあまり利用されない
// indicesは有効なindexから成るリストを返す

// 16.6.8 リストのリストから単層のリストへ: flatten
List(List(1, 2), List(3), List(4,5)).flatten

// 16.6.9 リストのジッパー的な操作: zipとunzip
// zipは2個のリストからタプルのリストを作る
// 要素の長さが異なる場合は切り捨てられる
val abcde = List("a", "b", "c", "d", "e")
val zipped = abcde.indices zip abcde
println(zipped)

// indicesと要素の対応は下記とほぼ等価(要素とindexが逆)
abcde.zipWithIndex

// unzipはタプルのリストをリストのタプルにかえられる
// List[(String, Int)] => (IndexedSeq[Int], IndexedSeq[String])
zipped.unzip

// 16.6.10 リストの表示: toStringとmkString
// toStringはリストの文字列表現を返す
// mkStringはprefix, postfix, separatorを指定してリストの要素を連結できる

// StringBuilderに結果を追加するaddStringというメソッドもある

// 16.6.11 リストの変換: iterator, toArray, copyToArray
// ArrayとListは相互に変換できる
abcde.toArray.toList

// copyToArrayはArrayの指定した位置にリストの要素をコピーすることができる
val arr = new Array[Int](10)
List(1, 2, 3).copyToArray(arr, 3)
arr
//val arr: Array[Int] = Array(0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
//val res9: Int = 3
//val res10: Array[Int] = Array(0, 0, 0, 1, 2, 3, 0, 0, 0, 0)

// iteratorメソッドはListからiteratorを生成できる
val iter = abcde.iterator
iter.next()
iter.next()

// 16.6.12 マージソート
def msort[T](less: (T, T) => Boolean)
            (xs: List[T]): List[T] = {
  def merge(xs: List[T], ys: List[T]): List[T] =
    (xs, ys) match {
      case (Nil, _) => ys
      case (_, Nil) => xs
      case (x:: xs1, y::ys1) =>
        if (less(x, y)) x :: merge(xs1, ys)
        else y :: merge(xs, ys1)
    }
  val n = xs.length / 2
  if (n == 0) xs
  else {
    val (ys, zs) = xs splitAt n
    merge(msort(less)(ys), msort(less)(zs))
  }
}
msort((x: Int, y:Int) => x < y) (List(5, 7, 1, 3))
val intsort = msort((x: Int, y:Int) => x < y) _
intsort(List(5, 7, 1, 3))