package com.example

import scala.collection.mutable

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