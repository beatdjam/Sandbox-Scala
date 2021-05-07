import scala.annotation.tailrec

// Appトレイトでエントリポイントを設定したもの
object Hello extends App {
  println("Hello, World!")
}

// mainメソッドでエントリポイントを設定したもの
object ScalaTour {
  def main(args: Array[String]): Unit = {
    val message = "Hello World"
    println(message)

    val n = 15
    fizzBuzz(n)
    fizzBuzzMatch(n)
    fizzBuzzRecursion(n)

    println(gcd(12, 30))
  }

  def fizzBuzz(n: Int): Unit = {
    for {i <- 1 to n} {
      if (i % 15 == 0) println("FizzBuzz")
      else if (i % 3 == 0) println("Fizz")
      else if (i % 5 == 0) println("Buzz")
      else println(i)
    }
  }

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

  def fizzBuzzRecursion(n: Int, i: Int = 1): Unit = {
    i match {
      case x if x % 15 == 0 => println("FizzBuzz")
      case x if x % 3 == 0 => println("Fizz")
      case x if x % 5 == 0 => println("Buzz")
      case x => println(x)
    }

    if (i < n) fizzBuzzRecursion(n, i + 1)
  }

  @tailrec
  def gcd(a: Int, b: Int): Int = {
    if (a % b == 0) b else gcd(b, a % b)
  }
}