package com.example

import org.mockito.ArgumentMatchers.any
import org.mockito.Mockito
import org.mockito.Mockito.{never, times, verify, when}
import org.mockito.exceptions.verification.WantedButNotInvoked
import org.scalatest.EitherValues.convertRightProjectionToValuable
import org.scalatest.OptionValues.convertOptionToValuable
import org.scalatest.TryValues.convertTryToSuccessOrFailure
import org.scalatest._
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.funsuite.{AnyFunSuite, AsyncFunSuite}
import org.scalatest.wordspec.AnyWordSpec

import scala.collection.mutable
import scala.util.Try

// AnyFunSuiteはxUnit形式のテストスタイル
// FunSpecやWordSpecより表現力が低い
class TestExampleTest extends AnyFunSuite {
  test("引数の人物名に対する挨拶が生成される") {
    val msg = TestExample.createMessage("John")
    assert(msg == "Hello, John!")
  }
  test("引数の人物名が空文字列の場合、IllegalArgumentExceptionが投げられる") {
    assertThrows[IllegalArgumentException] {
      TestExample.createMessage("")
    }
  }
}

// FunSpecはRSpecライクな記述が可能
// describeで説明をネストさせることができるので、構造的なふるまいの記述がしやすい
class TestExampleTestFunSpec extends AnyFunSpec {
  describe("挨拶") {
    describe("引数の人物名が空でない場合は") {
      it("挨拶が生成される") {
        val msg = TestExample.createMessage("John")
        assert(msg == "Hello, John!")
      }
    }

    describe("引数の人物名が空の場合は") {
      it("IllegalArgumentExceptionが投げられる") {
        assertThrows[IllegalArgumentException] {
          TestExample.createMessage("")
        }
      }
    }
  }
}

// WordSpecはspecs2ライクな記述が可能
// 英語で記述したときに自然な文章になるようDSLが設計されている
class TestExampleTestWordSpec extends AnyWordSpec {
  "挨拶処理の" when {
    "引数の人物名が空でない場合は" should {
      "挨拶が生成される" in {
        val msg = TestExample.createMessage("John")
        assert(msg == "Hello, John!")
      }
    }
    "引数の人物名が空の場合は" should {
      "IllegalArgumentExceptionが投げられる" in
        assertThrows[IllegalArgumentException] {
          TestExample.createMessage("")
        }
    }
  }
}


class Test extends AnyFunSuite {
  // org.scalatest.OptionValuesを利用してOptionに値を取得するメソッドを生やす
  val mappings = Map(
    "red" -> "#FF0000",
    "lime" -> "#00FF00"
  )

  test("red") {
    assert(mappings.get("red").value == "#FF0000")
  }

  test("green") {
    assert(mappings.get("green") == None)
  }

  // Try、Eitherも同じように値を取得するメソッドがある
  def parseInt(s: String): Try[Int] = Try(s.toInt)

  test("数値文字列を Int型の値にパースできる(Try)") {
    assert(parseInt("1").success.value == 1)
  }
  test("数値文字列を Int型の値にパースできる(Either)") {
    assert(parseInt("1").toEither.right.value == 1)
  }
}

class ConfigManagerBoilerplateTest extends AnyFunSuite {
  test("すでに設定キーが存在していた場合は設定値が上書きされる") {
    val configManager = new ConfigManager {
      override val envPrefix: String = "test"
      override val config: mutable.Map[String, String] = mutable.Map(
        "test.user" -> "John",
        "test.url" -> " https://example.com"
      )
    }

    configManager.upsertConfig("user", "Richard")
    assert(configManager.numOfConfig() == 2)
    assert(configManager.readConfig("user") == "Richard")
  }


  test("clearAllメソッドを実行すると設定値がすべて削除される") {
    val configManager = new ConfigManager {
      val envPrefix: String = "test"
      val config: mutable.Map[String, String] = mutable.Map(
        "test.user" -> "John",
        "test.url" -> " https://example.com"
      )
    }

    configManager.clearAll()
    assert(configManager.numOfConfig() == 0)
  }

  trait TestFixture {
    val configManager: ConfigManager = new ConfigManager {
      val envPrefix: String = "test"
      val config: mutable.Map[String, String] = mutable.Map(
        "test.user" -> "John",
        "test.url" -> " https://example.com"
      )
    }
  }


  test("すでに設定キーが存在していた場合は設定値が上書きされる(trait)") {
    new TestFixture {
      configManager.upsertConfig("user", "Richard")
      assert(configManager.numOfConfig() == 2)
      assert(configManager.readConfig("user") == "Richard")
    }
  }


  test("clearAllメソッドを実行すると設定値がすべて削除される(trait)") {
    new TestFixture {
      configManager.clearAll()
      assert(configManager.numOfConfig() == 0)
    }
  }
}

class ConfigManagerBeforeAndAfterTest extends AnyFunSuite with BeforeAndAfter with PrivateMethodTester {
  val configManager: ConfigManager = new ConfigManager {
    val envPrefix: String = "test"
    val config: mutable.Map[String, String] = mutable.Map.empty
  }

  before {
    configManager.config.addAll(
      mutable.Map(
        "test.user" -> "John",
        "test.url" -> " https://example.com"
      )
    )
  }
  after {
    configManager.clearAll()
  }

  test("すでに設定キーが存在していた場合は設定値が上書きされる(Fixture)") {
    configManager.upsertConfig("user", "Richard")
    assert(configManager.numOfConfig() == 2)
    assert(configManager.readConfig("user") == "Richard")
  }

  test("clearAllメソッドを実行すると設定値がすべて削除される(Fixture)") {
    configManager.clearAll()
    assert(configManager.numOfConfig() == 0)
  }
}

class PrivateMethodTest extends AnyFunSuite with PrivateMethodTester {
  class Foo {
    private def inc(i: Int): Int = i + 1
  }

  val method: PrivateMethod[Int] = PrivateMethod[Int]('inc)
  val result: Int = new Foo().invokePrivate(method(1))
  test("privateメソッドのテスト") {
    assert(result == 2)
  }
}

class AsyncCalculatorTest extends AsyncFunSuite {
  test("非同期テスト") {
    AsyncCalculator.div(10, 2).map { result => assert(result == 5) }
  }

  test("ArithmeticException") {
    recoverToSucceededIf[ArithmeticException] {
      AsyncCalculator.div(1, 0)
    }
  }
}


class SpreadsheetReaderTest extends AnyFunSuite {
  val mockedSpreadSheetReader: SpreadsheetReader =
    Mockito.mock(classOf[SpreadsheetReader])

  when(mockedSpreadSheetReader.readSheet("sheet_1"))
    .thenReturn(Some(Seq.empty))
  when(mockedSpreadSheetReader.readSheet("sheet_2"))
    .thenReturn(None)
  when(mockedSpreadSheetReader.isEmptySheet(any[String]))
    .thenReturn(true)

  test("readSheetの引数にwhenで指定されていない文字列を渡すとnull") {
    assert(mockedSpreadSheetReader.readSheet("sheet_other") == null)
  }

  test("readSheetの引数がsheet_1なら Some") {
    assert(mockedSpreadSheetReader.readSheet("sheet_1").value == Nil)
    verify(mockedSpreadSheetReader, times(1)).readSheet("sheet_1")
  }

  test("readSheetの引数がsheet_2ならNone") {
    assert(mockedSpreadSheetReader.readSheet("sheet_2") == None)
    verify(mockedSpreadSheetReader, times(1)).readSheet("sheet_1")
  }

  test("isEmptySheetの引数に任意の文字列を渡すとtrue") {
    assert(mockedSpreadSheetReader.isEmptySheet("sheet_other"))
    assert(mockedSpreadSheetReader.isEmptySheet("sheet_sample"))
    assert(mockedSpreadSheetReader.isEmptySheet("foo_bar"))

    verify(mockedSpreadSheetReader, times(3)).isEmptySheet(any[String])
    verify(mockedSpreadSheetReader, never()).readSheetNames
  }

  test("Mockito#verifyで検証が通らなかった場合はWantedButNotInvoked") {
    assertThrows[WantedButNotInvoked] {
      verify(mockedSpreadSheetReader, times(1)).readSheetId
    }
  }
}

class authServiceTest extends AnyFunSuite {
  val (john, jane, password) = ("john", "jane", "password")

  val mockedUserRepository: UserRepository = Mockito.mock(classOf[UserRepository])

  when(mockedUserRepository.findByName(john))
    .thenReturn(Some(User(1, john, password)))

  when(mockedUserRepository.findByName(jane))
    .thenReturn(None)

  val authService: AuthService = new AuthService(mockedUserRepository)

  test("ユーザーが見つかり、パスワードが一致した場合はtrue") {
    assert(authService.authenticate(john, password))
    verify(mockedUserRepository, never()).save(any[Long])
    verify(mockedUserRepository, times(1)).findByName(john)
  }

  test("ユーザーが見つからない場合はfalse") {
    assert(!authService.authenticate(jane, password))
    verify(mockedUserRepository, never()).save(any[Long])
    verify(mockedUserRepository, times(1)).findByName(jane)
  }
}