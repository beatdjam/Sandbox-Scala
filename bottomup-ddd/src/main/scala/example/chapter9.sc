case class UserName(value: String) {
  require(value.nonEmpty)
}
case class UserId(value: String)

case class User(id: UserId, name: UserName) {
  require(this.name.value.nonEmpty)

  def changeName(name: UserName): User = {
    this.copy(name = name)
  }

  override def equals(obj: Any): Boolean =
    obj match {
      case User(id, _) => id.value == this.id.value
      case _           => false
    }

  override def hashCode = (id, name).##
}

// ファクトリメソッドはコンパニオンオブジェクトに持たせるのが良さそうな気がする
object User {
  def create(name: UserName): User = {
    // ID採番の方法が変わったりしたらここに実装する
    // 本では直接DBアクセスしてたけど、リポジトリ介した方がよくない？
    // → あとの節で出てきた。著者的にはリポジトリは永続化と再構築が責務なので採番は異なるらしい。
    //   個人的にはドメインオブジェクトにDB操作が露出している方が嫌なのでリポジトリに生やそう
    User(UserId(java.util.UUID.randomUUID.toString), name)
  }
}
