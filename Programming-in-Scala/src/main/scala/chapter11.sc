// 11 Scalaのクラス階層
// Scalaでは全てのクラスがAnyを継承する
// Nothingは他のすべてのクラスのサブクラスになっている

// 11.1 Scalaのクラス階層
// AnyはAnyVal(値クラス)とAnyRef(参照クラス)を持っている
// 値クラスを継承するクラスは抽象クラスかつfinalなので、newでのインスタンス化ができない
// Unitはvoid型にほぼ対応している
// 値クラスの空間は互いにサブクラスにはなっていないが、暗黙の型変換が定義されている

// 11.2 プリミティブ型の実装方法
// Scalaの整数などは必要なときにはJavaのIntegerに透過的に変換される
// Javaの参照型の==は参照透過性を比較しているので等値かを判定するためにはequalsを使う必要があった
// Scalaは型の表現に左右されず、値が等しいかを判定する
// ScalaでJavaのように参照等価比較を行いたいときはeq, neを利用する必要がある
val x = "abcd".substring(2)
val y = "abcd".substring(2)
x == y // true
x eq y // false
x ne y // true

// 11.3 再開の２つの型
// Scalaの最下位にはscala.Nullとscala.Nothingがある
// Nullはnull参照の型で、すべての参照クラス(=AnyRefのサブクラス)のサブクラスで、値型とは互換性がない
// Nothingは他のすべての型のSubTypeであり、この型の値は存在しない(=異常終了したときにしか用いられない)
// Nothingがなぜ他のすべての型のSubTypeである必要があるのかというと下記のような関数を成り立たせるためである
def divide(x: Int, y: Int): Int = {
  if (y != 0) {
    x / y // Intを返す
  } else {
    sys.error("zero divide!") // Nothingを返すがIntのSubTypeなので成立する
  }
}
// throw new RuntimeException() などもNothingを返す