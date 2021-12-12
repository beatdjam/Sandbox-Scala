package repository

import domain.circle.{Circle, CircleId, CircleName}

trait CircleRepository {
  def save(circle: Circle): Unit
  def find(id: CircleId): Option[Circle]
  def find(name: CircleName): Option[Circle]
  def exists()
}
