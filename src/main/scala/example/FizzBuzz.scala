package example

import scala.annotation.tailrec

// FizzBuzzで様々な実装をしたサンプル

object FizzBuzz {
  // for式で単純に作る場合
  def fizzBuzz(n: Int): Unit = {
    for {i <- 1 to n} {
      if (i % 15 == 0) println("FizzBuzz")
      else if (i % 3 == 0) println("Fizz")
      else if (i % 5 == 0) println("Buzz")
      else println(i)
    }
  }

  // ifをmatchに置き換えた場合
  def fizzBuzzMatch(n: Int): Unit = {
    for {i <- 1 to n} {
      i match {
        case x if x % 15 == 0 => println("FizzBuzz")
        case x if x % 3 == 0 => println("Fizz")
        case x if x % 5 == 0 => println("Buzz")
        case x => println(x)
      }
    }
  }

  // 再帰で実行した場合
  @tailrec
  def fizzBuzzRecursion(n: Int, i: Int = 1): Unit = {
    i match {
      case x if x % 15 == 0 => println("FizzBuzz")
      case x if x % 3 == 0 => println("Fizz")
      case x if x % 5 == 0 => println("Buzz")
      case x => println(x)
    }
    if (i < n) fizzBuzzRecursion(n, i + 1)
  }

  // mapで実装した場合
  def toFizzBuzz(numbers: List[Int]): List[String] = numbers.map {
    case x if x % 15 == 0 => "FizzBuzz"
    case x if x % 3 == 0 => "Fizz"
    case x if x % 5 == 0 => "Buzz"
    case x => x.toString
  }
}
