package domain.circle

import domain.user.User

case class Circle(
    id: Option[CircleId] = None,
    name: CircleName,
    owner: User,
    members: Seq[User] = Nil
)

object Circle {
  def create(name: CircleName, owner: User): Circle =
    Circle(name = name, owner = owner)
}
