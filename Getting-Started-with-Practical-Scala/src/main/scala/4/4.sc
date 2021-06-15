import scala.collection.mutable

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
