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
