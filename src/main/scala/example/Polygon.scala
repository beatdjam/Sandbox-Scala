package example

abstract class Polygon(edges: List[Int]) {
  private val n = edges.length // n角形
  val area: Double //面積
}

object Polygon {
  def fromEdges(edges: List[Int]): Polygon = edges.length match {
    case 3 => new Triangle(edges)
    case x => ???
  }
}

class Triangle(edges: List[Int]) extends Polygon(edges) {
  private val a = edges.head
  private val b = edges(1)
  private val c = edges(2)

  val area: Double = {
    // ハロンの公式
    val s = (a + b + c) / 2.0
    math.sqrt(s * (s - a) * (s - b) * (s - c))
  }
}
