package domain.user

case class UserName(value: String) {
  require(value.nonEmpty)
}
