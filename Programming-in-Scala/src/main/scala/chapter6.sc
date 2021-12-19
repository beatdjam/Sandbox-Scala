// 6 関数型スタイルのオブジェクト

// 6.1 Rationalクラスの仕様
// ・有理数(分子と分母で表現される割合によって表現される数)をあらわす
// ・加減乗除などの分数の動作に含まれる演算を認める
// ・ミュータブルな常態を持たない

// 6.2 Rationalクラスの構築
// イミュータブルなオブジェクトのメリデメ
// メリット: 動作が予測しやすい、オブジェクトの受け渡しが安全、状態がかわらない・壊れない
// デメリット: オブジェクトの内部構造をコピーしなければならない場面がある

// class parameterがprimary constructorを形成する

// 6.3 toStringメソッドのオーバーライド

class Rational(n: Int, d: Int) {
  override def toString: String = s"$n / $d"
}

new Rational(1, 2)