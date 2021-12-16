package domain.circle

import domain.user.UserId

case class Circle(
    private val id: Option[CircleId] = None,
    name: CircleName,
    // Userの実体を持たせず、識別子によるコンポジションを行う
    owner: UserId,
    //    owner: User,
    // private val members: Seq[User] = Nil
    members: Seq[UserId] = Nil
) {
  def join(member: UserId): Circle = {
    // owner含めて30人のルールはCircle集約の制約なので
    require(!isFull, "circle is full")
    this.copy(members = members.appended(member))
  }
  def isFull: Boolean = countMembers >= 29
  def countMembers: Int = members.size + 1
}

object Circle {
  def create(name: CircleName, owner: UserId): Circle =
    Circle(name = name, owner = owner)
}
