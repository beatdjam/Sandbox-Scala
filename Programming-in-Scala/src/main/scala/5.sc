// 5 基本型と演算子

// 5.1  基本型
// Byte,Short,Int,Long,Charを整数値型
// Float,Doubleを数値型と呼ぶ
// 基本型のうちStringだけはjava.langパッケージに属していて、その他は全部scalaパッケージに含まれている

// 5.2 リテラル
// Scalaシェルは整数値の初期化のためにどの形式のリテラル表現を使っても必ず10進数で表示する

// raw string
// """で囲んだ文字列はraw stringとして扱える(Kotlinと同じ？)
// 各行の先頭にパイプ文字を入れてstripMarginメソッドを呼び出すとパイプの位置を行頭として扱うことができる
println(
  """Hello,
     World
""")
println(
  """|Hello,
     |World
""".stripMargin)

// シンボルリテラル
// `ident　のような形で書かれるものをシンボルリテラルと呼ぶ
// シンボルリテラルは英数字から構成される任意の識別子である
// 最新のScalaではSymbol("ident")のように記述する

// シンボルリテラルは、同じ値を扱うときに同じ実体を参照する
// 識別子として文字列を定義したいときなどに使うっぽい？

// 文字列補間
// s文字列補間子(string interpolator)を使って変数を展開した文字列を生成する
val name = "Taro"
println(s"Hello, $name!")
println(s"Hello, ${name.toUpperCase}!")
// 他にもraw, fがある
// rawは文字列リテラルのエスケープシーケンスを認識しない(=そのままの文字列を出力する)
println(s"string\\\\")
println(raw"raw\\\\")
// fはprintfと同じ形式で後ろにつけた文字列に応じて書式設定を行うことができる
// 何もつけなければsと同じように振る舞う
println(f"${math.Pi}%.10f")
println(f"${math.Pi}")
println(s"${math.Pi}")
// 文字列指定子はライブラリやユーザーが定義することもできる


