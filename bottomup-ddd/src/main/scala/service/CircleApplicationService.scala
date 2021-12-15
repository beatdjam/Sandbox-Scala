package service

import domain.circle.{Circle, CircleId, CircleName}
import domain.circle.service.CircleService
import domain.user.UserId
import repository.{CircleRepository, UserRepository}

class CircleApplicationService(
    circleRepository: CircleRepository,
    circleService: CircleService,
    userRepository: UserRepository
) {
  // 本来は一連の処理をスコープにしたTransaction管理入れる
  def create(ownerId: UserId, name: CircleName): Unit = {
    val owner =
      userRepository
        .find(ownerId)
        .getOrElse(throw new IllegalArgumentException("owner is not found"))
    val circle = Circle.create(name, owner.id)
    if (circleService.exists(circle)) {
      throw new IllegalArgumentException("same name circle is exist")
    }

    circleRepository.save(circle)
  }

  def join(userId: UserId, circleId: CircleId): Unit = {
    val member = userRepository
      .find(userId)
      .getOrElse(throw new IllegalArgumentException("user is not found"))

    val circle = circleRepository
      .find(circleId)
      .getOrElse(throw new IllegalArgumentException("circle is not found"))

    // このチェックがあり、Serviceでmemberの値を追加しているのは集約が破れている
    // require(circle.members.size < 29, "circle is full")
    // val joinedCircle = circle.copy(members = circle.members.appended(member))
    val joinedCircle = circle.join(member.id)
    circleRepository.save(joinedCircle)
  }
}
