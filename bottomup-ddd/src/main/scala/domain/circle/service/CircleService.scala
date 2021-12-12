package domain.circle.service

import domain.circle.Circle
import repository.CircleRepository

class CircleService(circleRepository: CircleRepository) {
  def exists(circle: Circle): Boolean = {
    circleRepository.find(circle.name) match {
      case Some(_) => true
      case _       => false
    }
  }
}
