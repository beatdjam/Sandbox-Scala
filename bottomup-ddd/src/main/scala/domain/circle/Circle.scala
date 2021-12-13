package domain.circle

import domain.user.User

case class Circle(
    private val id: Option[CircleId] = None,
    name: CircleName,
    owner: User,
    private val members: Seq[User] = Nil
) {
  def join(member: User): Circle = {
    // owner含めて30人のルールはCircle集約の制約なので
    require(!isFull, "circle is full")
    this.copy(members = members.appended(member))
  }
  def isFull: Boolean = members.size >= 29
}

object Circle {
  def create(name: CircleName, owner: User): Circle =
    Circle(name = name, owner = owner)
}
