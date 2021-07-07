// 1.1

// スクリプト言語のように簡潔で的確である
var capital = Map("US" -> "Washington", "France" -> "Paris")
capital += ("Japan" -> "Tokyo")

// 1.1.1 新しい型を作れる言語

// BigIntは組み込み型のように見えるが、標準ライブラリで定義されているクラスである
// もしBigIntがなくても、Scalaでは簡単に同じような実装を書ける
// 言語としてすべてをサポートするのではなく、自然に言語システムに溶け込む型やライブラリを定義できるようにしている
def factorial(x: BigInt): BigInt =
  if (x == 0) 1 else x * factorial(x - 1)

// 1.1.2 新しい制御構造を作れる言語
// Akkaのアクターモデルを表現するメッセージ送信(!)やreceiveブロックが独立したライブラリとして実装されているということ
// つまり、開発者が自分の望む形で言語を拡張していくことができる

// 1.2 Scalaがスケーラブルな理由

// 1.2.1 Scalaはオブジェクト指向
// すべての値はオブジェクトであり、すべての演算はメソッド呼び出しなので、Scalaは純粋なオブジェクト指向である

// トレイトはinterfaceのようであるが、メソッドやフィールドをもつことができる
// これらをmix-inすることで、クラスに様々な機能・ふるまいを付与することができる

// 1.2.2 Scalaは関数型言語

// 関数型言語の特徴は、関数をfirst-class valueとして扱うこと、値を変更せず入力を出力に写像すべきという考え
// メソッドはどんな副作用も持ってはならない、イミュータブルで参照透明な実装を推奨されている
// Scalaでは手続き型のように書くこともできるが、このようなスタイルが推奨されている

// 1.3 Scalaを選ぶ理由

// 1.3.1 互換性
// Javaとシームレスに相互運用できるように設計されている。
// JavaからScalaの拡張された型のメソッドを呼ぶ場合は暗黙の型変換が適用される

// 1.3.2 簡潔さ
// ボイラープレートを最低限にしたJavaの半分以下のコード量

// 1.3.3 高レベルの抽象
// 抽象度の高さ、新しい制御構造を表現できること
// 下記のように述語(predicate)を与えることで値のテストができる
// 結果の型がBooleanとなるような関数リテラルは述語と呼んで良いらしい
val name = "Test"
val nameHasUpperCase = name.exists(_.isUpper)

// 1.3.4 高度な静的型付け
