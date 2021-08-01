val add = (x: Int, y: Int) => x + y
val addCurried = (x: Int) => (y: Int) => x + y
add(100, 200)
val curried = addCurried(100)
curried(200)

import scala.io.Source

def withFile[A](filename: String)(f: Source => A): A = {
  val s = Source.fromFile(filename)
  try {
    f(s)
  } finally {
    s.close()
  }
}
def printFile(filename: String): Unit = {
  withFile(filename) { file =>
    file.getLines().foreach(println)
  }
}