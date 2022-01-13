// 14 アサーションとテスト
// 14.1 アサーション
// assertに与えた引数が条件を満たさない場合にAssertionErrorを投げる
def divide(num: Long, denom: Long) : Long = {
  assert(denom != 0, "Error!")
  num / denom
}

// ensuringメソッドを使って結果値に対するアサーションをすることもできる
val a = 1
val b = 0
val c = 1

(1 - 1) ensuring (_ >= 0) // ensuringに渡るのがtrueなので計算結果が返却される
(0 - 1) ensuring (_ >= 0) // AssertionErrorになる