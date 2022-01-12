// 13 パッケージとインポート
// 13.1 パッケージにコードをまとめる

// 下記3つの名前空間は等価
// package bobsrockets.navigation
// class Navigator

// package bobsrockets.navigation {
//     class Navigator
// }

// package bobsrockets {
//     package navigation {
//          class Navigator
//     }
// }

// 13.2 関連コードへの簡略なアクセス
// 同じパッケージのコードは簡略なアクセスができる
// 異なるパッケージのコードはpackage名を含んだアクセス(bobsrockets.navigation.Navigator)などにする

// 13.3 インポート
// import節を使って異なるパッケージを読み込むことができる
// importすると同じパッケージにあるのと同じようにパッケージ名を省いて呼び出す事ができる
// 単一のクラスをインポートする場合と、_を用いたオンデマンドインポートがある

// Scalaのimportは任意の場所に書けて、オブジェクトも参照でき、別名をつけることができる

// import Fruits.{Apple, Orange}

// import Fruits.{Apple => McIntosh, Orange}

// import Fruits.{_} = import Fruits._と同じ

// import Fruits.{Apple => _, _} Appleだけを隠す

// import Fruits.{Apple => McIntosh, _}  Appleだけを別名にする

// 13.4 暗黙のインポート
// 下記は暗黙的にインポートされている
// java.lang._
// scala._
// Predef._

// 13.5 アクセス修飾子
// private, protectedをつけられる

// 13.5.1 privateはスコープ内からのみアクセスできる
// 13.5.2 protectedは自身とそのサブクラスからのみ呼び出せる
// 13.5.3何もつけないとpublicになる

// 13.5.4 アクセス保護のスコープ
