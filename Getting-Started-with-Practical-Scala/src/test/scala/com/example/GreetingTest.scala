package com.example

import org.scalatest.{FunSpec, FunSuite, WordSpec}

// FunSuiteはxUnit形式のテストスタイル
// FunSpecやWordSpecより表現力が低い
class GreetingTest extends FunSuite {
  test("引数の人物名に対する挨拶が生成される") {
    val msg = Greeting.createMessage("John")
    assert(msg == "Hello, John!")
  }
  test("引数の人物名が空文字列の場合、IllegalArgumentExceptionが投げられる") {
    assertThrows[IllegalArgumentException] {
      Greeting.createMessage("")
    }
  }
}

// FunSpecはRSpecライクな記述が可能
// describeで説明をネストさせることができるので、構造的なふるまいの記述がしやすい
class GreetingTestFunSpec extends FunSpec {
  describe("挨拶") {
    describe("引数の人物名が空でない場合は") {
      it("挨拶が生成される") {
        val msg = Greeting.createMessage("John")
        assert(msg == "Hello, John!")
      }
    }

    describe("引数の人物名が空の場合は") {
      it("IllegalArgumentExceptionが投げられる") {
        assertThrows[IllegalArgumentException] {
          Greeting.createMessage("")
        }
      }
    }
  }
}

// WordSpecはspecs2ライクな記述が可能
// 英語で記述したときに自然な文章になるようDSLが設計されている
class GreetingTestWordSpec extends WordSpec {
  "挨拶処理の" when {
    "引数の人物名が空でない場合は" should {
      "挨拶が生成される" in {
        val msg = Greeting.createMessage("John")
        assert(msg == "Hello, John!")
      }
    }
    "引数の人物名が空の場合は" should {
      "IllegalArgumentExceptionが投げられる" in
        assertThrows[IllegalArgumentException] {
          Greeting.createMessage("")
        }
    }
  }
}