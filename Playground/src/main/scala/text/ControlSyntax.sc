var age: Int = 5
var isSchoolStarted: Boolean = false

if((1 to 6).contains(age)) println("幼児です") else println("幼児ではありません")

def loopFrom0To9(): Unit = {
  var i = 0
  do {
    println(i)
    i += 1
  } while(i < 10)
}
loopFrom0To9()

//for ( a <- 1 to 1000; b <- 1 to 1000; c <- 1 to 1000 if a * a == b * b + c * c) yield {
//  println(s"a: $a, b: $b, c: $c")
//}

//for (i <- 1 to 1000) {
//  val s = new scala.util.Random(new java.security.SecureRandom()).alphanumeric.take(5).toList match {
//    case List(a, b, c, d, _) => List(a, b, c, d, a)
//  }
//  println(s)
//}