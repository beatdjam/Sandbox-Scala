package domain.circle

case class CircleName(value: String) {
  require(value.nonEmpty)
  require(value.length >= 3)
  require(value.length <= 20)
}
