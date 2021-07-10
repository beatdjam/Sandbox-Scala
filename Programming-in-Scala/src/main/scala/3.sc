// 3.1 配列の型のパラメーター化
// valは変数の再代入は行えないが、変数が参照するオブジェクトは可変な場合もある(ミュータブルなオブジェクトなど)
val greetStrings = new Array[String](3)
greetStrings(0) = "Hello"
greetStrings(1) = ", "
greetStrings(2) = "world!\ns"

for (i <- 0 to 2)
  print(greetStrings(i))

// Scalaには演算子オーバーロードが無いが、+,-,*,-などをメソッド名で使うことができる
// 0 to 2 と (0).to(2)が等価なことからわかるように、中置表現として利用することで演算子を実現している

// Scalaの配列アクセスに丸括弧を使うのも、その変数がもつapplyメソッドに置き換わっているということになる。
// greetStrings(0)はgreetStrings.apply(0)と等価になる

// 丸括弧に囲まれた1個以上の引数を伴う変数への代入はupdateメソッドの呼び出しに置き換えられる
// greetStrings(0) = "Hello" と greetStrings.update(0, "Hello") も等価になる

// 下記のようにより簡潔に配列の初期化を行う方法もある
// Arrayのコンパニオンオブジェクトが持ったArray.applyに任意の数の引数を渡すことで初期化できるようになっている
val numNames = Array("zero", "one", "two")

// 3.2 リストの使い方
// Scalaにはイミュータブルなコレクション用の型としてListを持っている

val oneTwo = List(1, 2)
val threeFour = List(3, 4)
// Listには2つリストを結合する「:::」というメソッドがあるので下記のように書ける
val oneTwoThreeFour = oneTwo ::: threeFour
val oneTwoThreeFour2 = threeFour.:::(oneTwo)

// 先頭に新しい要素を加えたリストを生成して返す「::」メソッドもある
// このメソッドはconstructからconsと呼ばれる
// メソッド名がコロンで終わる場合は演算子の右側の型のメソッドが呼び出される。
val twoThree = List(2, 3)
val oneTwoThree = 1 :: twoThree
val oneTwoThree2 = twoThree.::(1)

// 重要なListメソッドのメモ
// ・List()/Nil は空リストを表す
// ・1 :: 2 :: 3 :: Nil のようにすると、右側から解決されていくので、空リストの先頭に3, 2, 1と追加したものが得られる
// ・List.count => 与えられた条件式にマッチする要素の個数
// ・List.forall => 与えられた条件式にすべての要素がマッチするかを返す
// ・List.init => 末尾の要素を除いたリストを返す
// ・List.mkString => リストの要素をつなげた文字列を生成する

// Listの末尾への追加はサイズに比例して遅くなるが、先頭への挿入は劣化しない
// そのため、先頭に要素を挿入してreverseしたり、ListBufferを使うなどで要素の追加を扱う事が多い

// 3.3 タプルの使い方
val pair: (Int, String) = (99, "Luftballons")
pair._1 // 1つめの要素が0で無いことに注意
pair._2

// Tupleの実体はTuple1 ~ 22の型になる
// 各要素で異なる型を格納できるので、添字でアクセスできるようになっていない

// 3.4 集合とマップの使い方
// ScalaのSetとMapはミュータブル・イミュータブルで同じ名称だけどパッケージが違う
var jetSet = Set("Boeing", "Airbus")
// イミュータブルなSetに+=しているが、起きていることは
// jetSet = jetSet + "Lear" のように新しいSetを代入してるのと等価
jetSet += "Lear"
println(jetSet.contains("Cessna"))

val romanNumeral = Map(
  1 -> "I", 2 -> "II", 3 -> "III"
)

// 3.5 関数型のスタイルの見分け方
// 関数型のスタイルに近づけるためには、イミュータブルな値を使い、副作用を抑える
// とはいえ、命令形のスタイルが適している場面もある
// 明確な理由が有るときに限り命令形で書く、というスタンスであるべき
// 命令形でミュータブルな値を使うときはスコープを狭くして影響を最小限にする

