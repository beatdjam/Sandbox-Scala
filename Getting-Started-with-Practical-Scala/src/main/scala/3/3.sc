import java.io.File

// Javaのnull返却をラップするだけのメソッド
def myListFiles(directory: File): Option[Array[File]] = Option(directory.listFiles())

// ファイル一覧を出力する
def printFileList(l: Option[Array[File]]): Unit = {
  l match {
    case Some(value) => value.foreach(v => println(v))
    case None => println("ファイルが存在しません")
  }
}

// 存在しない場合はNoneになる
printFileList(myListFiles(new File("存在しないディレクトリ")))
// 成功する場合
printFileList(myListFiles(new File(".")))

def plus(option1: Option[Int], option2: Option[Int]): Option[Int]
= option1.flatMap(i => option2.map(j => i + j))

plus(Option(2), Option(3))
plus(Option(2), None)
plus(None, Option(2))

def plusByForExpression(option1: Option[Int], option2: Option[Int]): Option[Int]
= for (i <- option1; j <- option2) yield i + j

plusByForExpression(Option(2), Option(3))
plusByForExpression(Option(2), None)
plusByForExpression(None, Option(2))

plusByForExpression(Option(2), Option(3)).getOrElse(0)
plusByForExpression(Option(2), None).getOrElse(0)