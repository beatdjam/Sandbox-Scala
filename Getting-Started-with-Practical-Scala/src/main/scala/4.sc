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