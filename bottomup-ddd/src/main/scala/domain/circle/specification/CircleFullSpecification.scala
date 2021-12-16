package domain.circle.specification

import domain.circle.Circle
import repository.UserRepository

class CircleFullSpecification(private val repository: UserRepository) {
  def isSatisfiedBy(circle: Circle): Boolean = {
    val users = repository.list(circle.members)
    val premiumUserCount = users.count(_.isPremium);
    val circleUpperLimit = if (premiumUserCount < 10) 30 else 50
    circle.countMembers >= circleUpperLimit
  }
}
