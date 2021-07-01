package com.example

import scala.collection.mutable
import scala.concurrent.{ExecutionContext, Future}

object TestExample {
  def createMessage(targetName: String): String = {
    require(targetName.nonEmpty)
    s"Hello, $targetName!"
  }
}

trait ConfigManager {
  val envPrefix: String
  val config: mutable.Map[String, String]

  def readConfig(key: String): String
  = config(s"$envPrefix.$key")

  def upsertConfig(key: String, value: String): Unit
  = config.update(s"$envPrefix.$key", value)

  def numOfConfig(): Int = config.size

  def clearAll(): Unit = config.clear()
}

// 非同期サンプル用
object AsyncCalculator {
  def div(i: Int, j: Int)(implicit ec: ExecutionContext): Future[Int] = Future {
    i / j
  }
}

// Mockitoサンプル用
case class Spreadsheet(id: String, sheets: Map[String, Spreadsheet.Sheet])

object Spreadsheet {
  type Sheet = Seq[Seq[String]]
}

class SpreadsheetReader(spreadsheet: Spreadsheet) {
  def readSheetId: String = spreadsheet.id

  def readSheetNames: Seq[String] = spreadsheet.sheets.keys.toSeq

  def readSheet(sheetName: String): Option[Spreadsheet.Sheet] =
    spreadsheet.sheets.collectFirst {
      case (name, sheet) if name == sheetName => sheet
    }

  def isEmptySheet(sheetName: String): Boolean =
    readSheet(sheetName).exists(_.isEmpty)
}

// Mockitoサンプル用2
case class User(id: Long, name: String, password: String)

trait UserRepository {
  def save(id: Long): Unit

  def findById(id: Long): Option[User]

  def findByName(name: String): Option[User]
}

class AuthService(userRepository: UserRepository) {
  def authenticate(userName: String, password: String): Boolean =
    userRepository
      .findByName(userName)
      .exists(_.password == password)
}