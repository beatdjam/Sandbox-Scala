package domain.user

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

  override def hashCode: Int = (id, name).##
}

object User {
  def create(name: UserName): User = {
    User(UserId(java.util.UUID.randomUUID.toString), name)
  }
}
