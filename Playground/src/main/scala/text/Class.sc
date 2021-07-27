class Point3D(val x: Int, val y: Int, val z: Int)

val p = new Point3D(10, 20, 30)
println(p.x) // 10
println(p.y) // 20
println(p.z) // 30

abstract class Shape {
  def area: Double
}

class Rectangle(w: Double, h: Double) extends Shape {
  def area: Double = w * h
}

class Circle(r: Double) extends Shape {
  def area: Double = Math.pow(r, 2) * Math.PI
}


var shape: Shape = new Rectangle(10.0, 20.0)
println(shape.area)
shape = new Circle(2.0)
println(shape.area)