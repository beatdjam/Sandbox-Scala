case class UserName(value: String) {
  require(value.nonEmpty)
}
case class UserId(value: String)

case class User(id: UserId, name: UserName) {
  def this(name: UserName) = {
    require(name.value.nonEmpty)
    User(UserId(java.util.UUID.randomUUID.toString), name)
  }

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
  def delete(user: User): Boolean
  def find(id: UserId): Option[User]
  def findByUserName(name: UserName): Seq[User]
}

class UserService(private val userRepository: UserRepository) {
  def exists(user: User): Boolean = userRepository.findByUserName(user.name).nonEmpty
}

class UserApplicationService(
    private val userRepository: UserRepository,
    private val userService: UserService
) {
  def register(name: String): Unit = {
    val user = new User(UserName(name))
    if (userService.exists(user)) {
      // 適当なException
      throw new Exception("重複エラー")
    }
    userRepository.save(user)
  }

  // ドメインオブジェクトを公開する作りの場合
  def get(userId: String): Option[User] = {
    val targetId = UserId(userId)
    userRepository.find(targetId)
  }
}
