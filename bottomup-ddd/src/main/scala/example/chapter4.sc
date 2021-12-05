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

// ドメインオブジェクト自身が持つと不自然になってしまう振る舞いを切り出すService
// →ドメインサービス
// 入出力は極力ドメインサービスでは扱わないほうがわかりやすい
// また、具体の永続化にまつわるロジックはRepositoryに記述される
class UserService {
  def exists(user: User): Boolean = {
    // ここに存在確認の処理が入る
    false
  }
}
