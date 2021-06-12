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

reverseByFoldLeft(Seq(1,2,3))
reverseByFoldRight(Seq(1,2,3))

// = Sum
Seq(1,2,3).reduceLeft(_ + _)
Seq(1,2,3).reduceRight(_ + _)

Seq(1,2,3,1,2).toSet