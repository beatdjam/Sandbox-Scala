case class UserName(value: String) {
  require(value.nonEmpty)
}
case class UserId(value: String)

case class User(id: UserId, name: UserName) {

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

trait UserRepository {
  def save(user: User): Boolean
  def find(id: UserId): Option[User]
}

class UserRepositoryImpl() extends UserRepository {
  def save(user: User): Boolean = {
    // 保存の実処理
    ???
  }
  def find(id: UserId): Option[User] = {
    // ここに存在確認の処理が入る
    ???
  }
}

class UserService(private val userRepository: UserRepository) {
  def exists(id: UserId): Boolean = userRepository.find(id).isDefined
}
