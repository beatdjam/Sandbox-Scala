package domain.circle

case class CircleId(value: String) {
  require(value.nonEmpty)
}
