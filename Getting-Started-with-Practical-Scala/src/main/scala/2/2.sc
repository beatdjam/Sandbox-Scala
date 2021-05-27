// 複数行リテラル
"""
This is
a multiline String
literal
"""

// 区切り文字 | を指定することで字下げを削除できる
// stripMarginに渡す引数で区切り文字を変えることができる
"""
  |This is
  |a multiline String
  |literal
""".stripMargin

// 型メンバを使ってタプルに別名をつける
type PointTuple = (Int, Int, Int)
val p: PointTuple = (60, 70, 80)
println(p)

class Point(val x: Int, val y: Int) {
  def distance(that: Point): Int = {
    val xDiff = math.abs(this.x - that.x)
    val yDiff = math.abs(this.y - that.y)
    math.sqrt(xDiff * xDiff + yDiff * yDiff).toInt
  }

  // classに対するplusはinfixや演算子overrideではない？
  def +(that: Point): Point = new Point(x + that.x, y + that.y)
}