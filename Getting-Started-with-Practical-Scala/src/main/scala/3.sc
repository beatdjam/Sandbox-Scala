import java.io.File
import scala.util.Try

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

// 変位指定アノテーション
class Animal

class Human extends Animal

// 共変(サブタイプ関係を引き継ぐ)
class CoVariantType[+A](a: A)

val myHuman: CoVariantType[Human] = new CoVariantType(new Human)
val myAnimal: CoVariantType[Animal] = myHuman

// 反変(サブタイプ関係が逆転する)
class ContraVariantType[-A](a: A)

val myAnimal: ContraVariantType[Animal] = new ContraVariantType(new Animal)
val myHuman: ContraVariantType[Human] = myAnimal

// 非変(サブタイプを許容しない)
class InVariantType[A](a: A)

// Either
def fileSize(file: File): Either[String, Long] =
  if (file.exists()) Right(file.length()) else Left("File not exists")

// foreach使うとRightを取り出す
val r: Either[String, Int] = Right(100)
r.foreach(println)
val l: Either[String, Int] = Left("Error")
l.foreach(println)
l.left.foreach(println)

// Try
def div(a: Int, b: Int): Try[Int] = Try(a / b)
div(10, 3).foreach(println)
div(10, 0).failed.foreach(println)
div(10, 0).recover { case _: ArithmeticException => 0 }.foreach(println)
div(10, 0).recoverWith { case _: ArithmeticException => Try(0) }.foreach(println)
