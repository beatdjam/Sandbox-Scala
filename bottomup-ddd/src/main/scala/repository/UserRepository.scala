package repository

import domain.user.{User, UserId, UserName}

trait UserRepository {
  def save(user: User): Boolean
  def delete(user: User): Boolean
  def find(id: UserId): Option[User]
  def findByUserName(name: UserName): Seq[User]
}
