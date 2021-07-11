// 4 クラスとオブジェクト
// 4.1 クラス、フィールド、メソッド

// クラスのもつフィールドやメソッドはまとめてメンバーと呼ばれる
// Scalaではデフォルトのアクセスレベルはpublic

// 明示的なreturnがなければ、メソッド内で計算された最後の値を返す
// return文を書かないことが推奨されている
// メソッドを構成する要素が単一の結果式である場合、中括弧を省くこともできる
// publicなメソッドの返り値の型は明示したほうがわかりやすいとされているらしい。

// 結果がUnitの場合、副作用を目的としたメソッドとみなされる
// 副作用のみを目的として実行されるメソッドは手続き(procedure)と呼ばれる

// 4.2 セミコロン推論
// Scalaでは1行が1つの文を構成している場合に、文の末尾のセミコロンは省略して良い

// 下記のケースの場合はセミコロンの推論が働かない
// ・行末がピリオドや中置演算子など、文法的に認められていないもの
// ・次の行の冒頭が文の先頭として認められないもの
// ・括弧や角括弧が閉じられず行末がきていること

// 4.3 シングルトンオブジェクト
// Scalaではクラスが静的メンバーを持てない
// その代わりにシングルトンオブジェクトを定義することができる

// 存在するクラスと同名のシングルトンオブジェクトはコンパニオンオブジェクトとなる
// コンパニオンオブジェクトに対応するクラスはコンパニオンクラスと呼ばれ、相互にprivateメンバーを参照できる
// コンパニオンクラスに対応しないシングルトンオブジェクトはスタンドアロンオブジェクトとよばれ、Utilなどに使われたりする

// シングルトンオブジェクトは型を定義するものではない
// ただし、一線級のオブジェクトなので、クラスやトレイトをmix-inすることができる？らしい
// シングルトンオブジェクトはインスタンスをnewキーワードで生成しないのでパラメータを渡すことができない
// 個々のシングルトンオブジェクトは自動生成クラスのインスタンスとして表現される

// 4.4 Scalaアプリケーション
// Scalaプログラムを実行するには、mainメソッドをもつスタンドアロンシングルトンオブジェクトを定義する必要がある
// mainメソッドはArray[String]を受け取り、Unitを返り値とするもの

// Scalaではファイル内の記述に関わらず好きなファイル名をつけることができるが、Javaと同様に格納しているクラスと同じ名前をつけられることが多い

// 4.5 Appトレイト
// シングルトンオブジェクトにextends Appを付与することで、mainメソッドを書かずに直接エントリポイントの処理を書くことができる
// 4.4の記述の省略形のようなもの