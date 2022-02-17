import scalaz.Scalaz._
// Equal
// === =/= assert_===
// 標準のsyntaxは異なる型でも比較できてしまう
1 == "2"

// Scalazは異なる型を比較するとCEになる
// 1 === "2"
1 === 1
1 === 2

// =/=はNot Equal
1.some =/= 2.some

// Not Equalのとき、Runtime errorになる
// 1 assert_=== 2

// Order
// lt gt lte gte min max

// 標準のsyntaxは異なる型でも比較できてしまう
1 > 2.0

// Scalazは異なる型を比較するとCEになる
// 1 gt 2.0

// ?|?は比較結果を返す
1 ?|? 2 // LT

// Show
// Show型のインスタンスを文字列として表現する
3.show
3.shows
// showの結果をコンソール出力
"hoge".println

// Enum
// |-> -+- --- from fromStep pred predx succ succx |--> |-> |==> |=>
'a' to 'e'
'a' |-> 'e'
3 |=> 5
'B'.succ
