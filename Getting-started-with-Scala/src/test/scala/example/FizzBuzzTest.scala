package example

import org.json4s.DefaultFormats
import org.json4s.jackson.Serialization
import org.scalatest.FlatSpec
import sbt.io.IO

import java.io.File

class FizzBuzzTest extends FlatSpec {

  // テストで実行する仕様を書く
  def createSourceJSONAndThenFizzBuzzFromJSON(n: Int): Unit = {
    implicit val formats = DefaultFormats
    val sourceFile = new File("sample.json")
    val destinationFile = new File("fizzBuzz.json")
    FizzBuzz.createSourceJson(n, sourceFile)
    FizzBuzz.fizzBuzzFromJson(sourceFile, destinationFile)

    val json = Serialization.read[FizzBuzzHolder](IO.read(destinationFile))
    json.fizzBuzz.zipWithIndex.foreach(pair => {
      pair._2 + 1 match {
        case x if x % 15 == 0 => assert(pair._1 == "FizzBuzz")
        case x if x % 3 == 0 => assert(pair._1 == "Fizz")
        case x if x % 5 == 0 => assert(pair._1 == "Buzz")
        case x => assert(pair._1 == x.toString)
      }
    })
  }

  // DSLでテストケースを書く
  s"`createSourceJSON` and `fizzBuzzFromJSON` (1 to 0)" should
    "throw IllegalArgumentException" in {
    assertThrows[IllegalArgumentException] {
      createSourceJSONAndThenFizzBuzzFromJSON(0)
    }
  }
  for {n <- Array(1, 15, 100)} {
    s"`createSourceJSON` and `fizzBuzzFromJSON` (1 to $n)" should
      "apply FizzBuzz to data from JSON file" in {
      createSourceJSONAndThenFizzBuzzFromJSON(n)
    }
  }

  // 失敗するテスト
  //  s"`createSourceJSON` and `fizzBuzzFromJSON` (1 to 0)" should
  //    "apply FizzBuzz to data from JSON file" in {
  //    createSourceJSONAndThenFizzBuzzFromJSON(0)
  //  }
}
