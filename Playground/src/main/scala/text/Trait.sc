trait A {
  val foo: String
}

trait B extends A {
  val bar = foo + "World" // Cでfooが初期化される前に呼ばれるからnullになる
  lazy val bar1 = foo + "World" // 遅延評価だから呼ばれたタイミングで固定される

  def bar2 = foo + "World" // 都度評価するから呼ばれたタイミングの値が出る
}

class C extends B {
  val foo = "Hello"

  def printBar(): Unit = println(bar)

  def printBar1(): Unit = println(bar1)

  def printBar2(): Unit = println(bar2)
}

class D extends {
  val foo = "Hello" // スーパークラスの初期化の前に呼び出される
} with B {
  def printBar(): Unit = println(bar)
}

(new C).printBar()
(new C).printBar1()
(new C).printBar2()
(new D).printBar()