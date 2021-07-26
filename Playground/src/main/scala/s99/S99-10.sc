import scala.annotation.tailrec

val ls = List(1, 1, 2, 3, 5, 8)

// P01
// わからなかったので答えを写経
object P01 {
  @tailrec
  def last[A](ls: List[A]): A = ls match {
    case h :: Nil => h
    case _ :: tail => last(tail)
    case _ => throw new NoSuchElementException
  }
}

ls.last
P01.last(ls)

// P02
// わからなかったので答えを写経
object P02 {
  @tailrec
  def penultimate[A](ls: List[A]): A = ls match {
    case h :: _ :: Nil => h
    case _ :: tail => penultimate(tail)
    case _ => throw new NoSuchElementException
  }

  def lastNthRecursive[A](n: Int, ls: List[A]): A = {
    @tailrec
    def lastNthR(count: Int, resultList: List[A], curList: List[A]): A = {
      curList match {
        case Nil if count > 0 => throw new NoSuchElementException
        case Nil => resultList.head
        case _ :: tail => lastNthR(
          count - 1,
          if (count > 0) resultList else resultList.tail,
          tail
        )
      }
    }

    if (n <= 0) throw new IllegalArgumentException
    else lastNthR(n, ls, ls)
  }
}

ls.init.last
P02.penultimate(ls)
P02.lastNthRecursive(2, ls)
P02.lastNthRecursive(6, ls)

object P03 {
  @tailrec
  def nthRecursive[A](n: Int, ls: List[A]): A = (n, ls) match {
    case (0, h :: _) => h
    case (n, _ :: tail) => nthRecursive(n - 1, tail)
    case (_, Nil) => throw new NoSuchElementException
  }
  // 最初に書いた実装
  // 0をパターンマッチで受け取ったほうが確かに綺麗だなと思った
  //  def nthRecursive[A](n: Int, ls: List[A]): A = ls match {
  //    case h :: _ if n == 0 => h
  //    case _ :: tail => nthRecursive(n - 1, tail)
  //    case Nil => throw new NoSuchElementException
  //  }
}

ls(2)
P03.nthRecursive(2, ls)
P03.nthRecursive(5, ls)

object P04 {
  def lengthRecursive[A](ls: List[A]): Int = {
    @tailrec
    def lengthR(n: Int, ls: List[A]): Int = ls match {
      case Nil => n
      case _ :: tail => lengthR(n + 1, tail)
    }

    lengthR(0, ls)
  }
}

ls.length
P04.lengthRecursive(ls)

object P05 {
  def reverse[A](ls: List[A]): List[A] = {
    @tailrec
    def reverseR(result: List[A], ls: List[A]): List[A] = ls match {
      case h :: tail => reverseR(h :: result, tail)
      case Nil => result
    }

    reverseR(Nil, ls)
  }
}

ls.reverse
P05.reverse(ls)

object P06 {
  def isPalindrome[A](ls: List[A]): Boolean = {
    @tailrec
    def reverseR(result: List[A], ls: List[A]): List[A] = ls match {
      case h :: tail => reverseR(h :: result, tail)
      case Nil => result
    }

    val result = reverseR(Nil, ls)
    ls == result
  }
}

val palindromeList = List(1, 2, 3, 2, 1)
palindromeList.reverse == palindromeList
P06.isPalindrome(palindromeList)
P06.isPalindrome(palindromeList :+ 3)

object P07 {
  // わからなくて答え見た
  def flatten(ls: List[Any]): List[Any] = ls flatMap {
    case ms: List[_] => flatten(ms)
    case e => List(e)
  }
}
// 組み込みのflattenは全部Traversableな要素じゃないと使えない？
List(List(1, 2), List(3, 4), List(5, 6)).flatten

P07.flatten(List(List(1, 1), 2, List(3, List(5, 8))))

object P08 {
  // 下記を書いたけど要件は重複の排除ではなくて、連続する重複の削除だった
  // toSetは順番を保持しないことがついでにわかった
  // def compress[A](ls: List[A]):List[A] = ls.toSet.toList

  // 以下回答の写経
  // 先頭の要素を取り出して、後続の配列から連続する同じ要素をdropしたものを再帰で渡す
  // 末尾最適ではない
  def compress[A](ls: List[A]): List[A] = ls match {
    case Nil => Nil
    case ::(h, tail) => h :: compress(tail.dropWhile(_ == h))
  }

  // 末尾最適
  def compressTailRecursive[A](ls: List[A]): List[A] = {
    @tailrec
    def compressR(result: List[A], curList: List[A]): List[A] = curList match {
      // 先頭の要素をresultの先頭に入れる(先頭に入れると要素の長さに関係なく処理時間が一定になる)
      // tailの連続する同じ要素を取り除いたものをcurListで渡す
      case h :: tail => compressR(h :: result, tail.dropWhile(_ == h))
      // 最後までたどり着いたらListを逆転させて返す
      case Nil => result.reverse
    }

    compressR(Nil, ls)
  }

  // Functional.
  def compressFunctional[A](ls: List[A]): List[A] =
    ls.foldRight(List[A]()) { (h, r) =>
      //      println(h)
      //      println(r)
      // hは処理中の要素、rは畳み込まれているresult
      // 結果が空のとき、または結果の先頭が異なる文字のときにリストに追加
      // それ以外は結果のみを返している
      if (r.isEmpty || r.head != h) h :: r
      else r
    }
}

P08.compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
P08.compressFunctional(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
