case class UserId(value: String)

case class User(id: UserId, name: String) {
  require(name.nonEmpty)

  // 属性の変更ができる
  def changeName(name: String): User = {
    this.copy(name = name)
  }

  // 識別子で同一性を表現する
  override def equals(obj: Any): Boolean =
    obj match {
      case User(id, _) => id.value == this.id.value
      case _           => false
    }

  override def hashCode = (id, name).##
}

// 名前を変えても同一性が維持されてる
val id = UserId("1")
val user = User(id, "one")
user == user.changeName("two")