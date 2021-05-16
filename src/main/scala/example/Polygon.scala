package example

// 抽象クラス、traitの振る舞いのサンプル

abstract class Polygon(edges: List[Int]) {
  val n: Int = edges.length // n角形
  val area: Double //面積
}

object Polygon {
  def fromEdges(edges: List[Int]): Option[Polygon] = edges.length match {
    case 3 => Some(new Triangle(edges))
    case _ => None
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

trait Color {
  val red: Int
  val green: Int
  val blue: Int

  def printColor(): Unit = println(s"$red-$green-$blue")
}

trait Blue extends Color {
  override val red: Int = 0
  override val green: Int = 0
  override val blue: Int = 255
}

trait Yellow extends Color {
  override val red: Int = 255
  override val green: Int = 255
  override val blue: Int = 0
}

trait Transparency {
  val alpha: Double
}

trait Frosted extends Transparency {
  override val alpha: Double = 0.5
}

class BlueFrostedTriangle(edges: List[Int]) extends Polygon(edges) with Blue with Frosted {
  private val a = edges.head
  private val b = edges(1)
  private val c = edges(2)

  val area: Double = {
    // ハロンの公式
    val s = (a + b + c) / 2.0
    math.sqrt(s * (s - a) * (s - b) * (s - c))
  }
}