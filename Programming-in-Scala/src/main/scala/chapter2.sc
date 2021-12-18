// 2. Scalaプログラミングの第一歩

// 明示的な型アノテーションはコンパイラ、後でコードを読む人間にとって役立つドキュメントとなる
// 適度に型推論させよう

// 「関数とは結果として値を生み出す式を定義したものだ」
// 関数が再帰的な場合は関数の結果の型を明示しなければならない

// 下記は等価
Seq("A", "B", "C").foreach(arg => println(arg))
Seq("A", "B", "C").foreach((arg: String) => println(arg))
Seq("A", "B", "C").foreach(println)
