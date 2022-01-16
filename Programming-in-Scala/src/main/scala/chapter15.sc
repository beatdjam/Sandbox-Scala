// 15 ケースクラスとパターンマッチ
// 15.1 単純な例
abstract class Expr
case class Var(name: String) extends Expr
case class Number(num: Double) extends Expr
case class UnOp(operator: String, srg: Expr) extends Expr
case class BinOp(operator: String, left: Expr, right: Expr) extends Expr

// 15.1.1 ケースクラス
// caseという修飾子をつけたクラスをcase classと呼ぶ
// この修飾子のついたクラスは暗黙的にいくつかの実装を追加される

// 1. 暗黙的にfactoryが定義される
val value = Var("x") // newがいらない

// 2. パラメータに暗黙的なvalプレフィックスが追加される
value.name

// 3. toString, hashCode, equalsの実装が追加される
// クラスとそのすべての引数の木構造を文字列化、ハッシュ化、比較する
// ==はequalsを利用するので、case classの場合は実際の構造とその値に基づいて比較される

// value.toString
// value.hashCode()
// value.equals()

// 4. copyメソッドが追加される
// 一部のパラメータが異なるインスタンスを作るときに、元のインスタンスから一部の値を置き換えたインスタンスを生成できる
val op = BinOp("+", Number(1), value)
op.copy(operator = "-")