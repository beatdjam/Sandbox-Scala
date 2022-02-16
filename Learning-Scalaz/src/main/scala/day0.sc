import scala.language.implicitConversions
//Scalaz は主に3つの部分から構成される:
//新しいデータ型 (Validation、NonEmptyList など)
//標準クラスの拡張 (OptionOps、ListOps など)
//実用上必要な全ての汎用関数の実装 (アドホック多相性、trait + implicit)

// 多相性ってなに？

// パラメータ多相
def head[A](xs: List[A]): A = xs(0)

// 派生型 (subtyping)
trait Plus[A] {
  def plus(a2: A): A
}
def plus[A <: Plus[A]](a1: A, a2: A): A = a1.plus(a2)

// アドホック多相
// implicitを使う
trait Plus[A] {
  def plus(a1: A, a2: A): A
}
def plus[A: Plus](a1: A, a2: A): A = implicitly[Plus[A]].plus(a1, a2)

// sumを一般化する
def sum(xs: List[Int]): Int = xs.foldLeft(0) { _ + _ }

// 1 Monoidを取り出す
object IntMonoid {
  def mappend(a: Int, b: Int): Int = a + b
  def mzero: Int = 0
}
def sum(xs: List[Int]): Int = xs.foldLeft(IntMonoid.mzero)(IntMonoid.mappend)

// 2 Monoidを外から渡す
trait Monoid[A] {
  def mappend(a1: A, a2: A): A
  def mzero: A
}
object IntMonoid extends Monoid[Int] {
  def mappend(a: Int, b: Int): Int = a + b
  def mzero: Int = 0
}
def sum[A](xs: List[A], m: Monoid[A]): A = xs.foldLeft(m.mzero)(m.mappend)

// 3 MonoidをImplicitにする
trait Monoid[A] {
  def mappend(a1: A, a2: A): A
  def mzero: A
}
object IntMonoid extends Monoid[Int] {
  def mappend(a: Int, b: Int): Int = a + b
  def mzero: Int = 0
}
implicit val intMonoid = IntMonoid
def sum[A](xs: List[A])(implicit m: Monoid[A]): A = xs.foldLeft(m.mzero)(m.mappend)

// 4 StringのMonoidを書く

trait Monoid[A] {
  def mappend(a1: A, a2: A): A
  def mzero: A
}
object Monoid {
  implicit val IntMonoid: Monoid[Int] = new Monoid[Int] {
    def mappend(a: Int, b: Int): Int = a + b

    def mzero: Int = 0
  }
  implicit val StringMonoid: Monoid[String] = new Monoid[String] {
    def mappend(a: String, b: String): String = a + b

    def mzero: String = ""
  }
}
def sum[A](xs: List[A])(implicit m: Monoid[A]): A = xs.foldLeft(m.mzero)(m.mappend)
sum(List(1, 2, 3, 4))
sum(List("a", "b", "c"))

// メソッド注入
trait MonoidOp[A] {
  val F: Monoid[A]
  val value: A
  def |+|(a2: A) = F.mappend(value, a2)
}
implicit def toMonoidOp[A: Monoid](a: A): MonoidOp[A] = new MonoidOp[A] {
  val F = implicitly[Monoid[A]]
  val value = a
}
