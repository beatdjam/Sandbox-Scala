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
