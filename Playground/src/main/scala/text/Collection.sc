def swapArray[T](arr: Array[T])(i: Int, j: Int): Unit = {
  val tmp = arr(i)
  arr(i) = arr(j)
  arr(j) = tmp
}

val arr = Array(1, 2, 3, 4, 5)
swapArray(arr)(2, 4)
println(arr.mkString(", "))

def joinByComma(start: Int, end: Int): String = (start to end).mkString(",")
println(joinByComma(1, 5))


def reverse[T](list: List[T]): List[T] = {
  list.foldLeft(Nil: List[T])((a, b) => b :: a)
}
reverse(List(1, 2, 3, 4, 5))

def sum(list: List[Int]): Int = list.foldRight(0)((a, b) => a + b)
sum(List(1, 2, 3, 4, 5))

def mul(list: List[Int]): Int = list.foldRight(1)((a, b) => a * b)
mul(List(1, 2, 3, 4, 5))
mul(Nil)

// 思いつかなかったので回答写し
def mkString[T](list: List[T])(sep: String): String = list match {
  case Nil => ""
  case x::xs => xs.foldLeft(x.toString){(x, y) => x + sep + y}
}