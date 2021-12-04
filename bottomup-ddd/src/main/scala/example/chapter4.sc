import scala.collection.mutable

// chapter3のコード
case class UserId(value: String)

case class User(id: UserId, name: String) {
  require(name.nonEmpty)
  def changeName(name: String): User = {
    this.copy(name = name)
  }

  override def equals(obj: Any): Boolean =
    obj match {
      case User(id, _) => id.value == this.id.value
      case _           => false
    }

  override def hashCode = (id, name).##
}

// ドメインオブジェクト自身が持つと不自然になってしまう振る舞いを切り出すService
// →ドメインサービス
class UserService {
  def exists(user: User): Boolean = {
    // ここに存在確認の処理が入る
    false
  }
}
