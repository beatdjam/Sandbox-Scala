import scala.collection.mutable
import scala.jdk.CollectionConverters._

val immutableSeq = scala.collection.immutable.Seq(1, 2, 3)
val mutableSeq = scala.collection.mutable.Seq(1, 2, 3)

immutableSeq.appendedAll(mutableSeq)
immutableSeq ++ mutableSeq
0 +: immutableSeq
immutableSeq :+ 4

immutableSeq.head
immutableSeq.headOption
immutableSeq.tail
immutableSeq.init
immutableSeq.last

// = Sum
Seq(1, 2, 3).foldLeft(0)((accumulator, element) =>
  accumulator + element
)
Seq(1, 2, 3).foldLeft(0)(_ + _)

// この場合値を先頭に追加するのでfoldLeftのほうが早い
def reverseByFoldLeft[A](seq: Seq[A])
= seq.foldLeft(Seq[A]())((a, e) => e +: a)
def reverseByFoldRight[A](seq: Seq[A])
= seq.foldRight(Seq[A]())((e, a) => a :+ e)

reverseByFoldLeft(Seq(1, 2, 3))
reverseByFoldRight(Seq(1, 2, 3))

// = Sum
Seq(1, 2, 3).reduceLeft(_ + _)
Seq(1, 2, 3).reduceRight(_ + _)

Seq(1, 2, 3, 1, 2).toSet

val set = Set("A", "B", "C")
set.contains("A")
// setのインスタンスのapplyはcontainsを呼ぶ
set("A")
set("D")

val mutableSet = mutable.Set(1, 2)
mutableSet += (3, 4)
mutableSet -= (2, 3)

val fruits = Map("Apple" -> 150, "Orange" -> 100)
fruits("Apple")
fruits.get("Banana")
fruits.getOrElse("Banana", "No Element")
fruits.keys
fruits.values
fruits.size

val fruits2 = fruits + ("Banana" -> 120)
val fruits3 = scala.collection.mutable.Map("Apple" -> 150, "Orange" -> 100) ++ Map("Lemon" -> 210)
"Lemon" -> 120

// JavaとScalaのcollectionの相互変換
val javaList = Seq(1, 2, 3).asJava
val javaMap = Map("Apple" -> 150, "Orange" -> 100).asJava
javaList.asScala // JavaのListからasScalaするとBufferになる
javaMap.asScala

val o1: Option[Int] = Some(1)
val o2: Option[Int] = Some(2)
val o3: Option[Int] = Some(3)

// 下のfor式と等価
o1.flatMap(i1 =>
  o2.flatMap(i2 =>
    o3.map(i3 =>
      i1 + i2 + i3
    )
  )
)

// ジェネレータの右辺の型が揃っていればyieldで結果を取得できる
for {
  i1 <- o1
  i2 <- o2
  i3 <- o3
} yield i1 + i2 + i3
for (i1 <- o1; i2 <- o2; i3 <- o3) yield i1 + i2 + i3

// yieldを使わなければ右辺の型が揃っていなくてもエラーにならない
for (o <- Option(1); l <- Seq(10, 20)) println(o + l)

val list1 = Seq(1, 2, 3)
val list2 = Seq("a", "b", "c")

// ガード節でジェネレータが生成する値を制限することができる
for {
  e1 <- list1 if e1 % 2 == 1
  e2 <- list2
} println((e1, e2))