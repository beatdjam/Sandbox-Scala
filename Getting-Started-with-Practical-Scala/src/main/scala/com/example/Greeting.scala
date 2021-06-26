package com.example

object Greeting {
  def createMessage(targetName: String): String = {
    require(targetName.nonEmpty)
    s"Hello, $targetName!"
  }

}
