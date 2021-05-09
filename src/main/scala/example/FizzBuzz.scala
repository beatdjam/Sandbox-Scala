package example

object FizzBuzz {
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
}
