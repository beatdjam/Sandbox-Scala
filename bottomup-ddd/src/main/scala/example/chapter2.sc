case class FullName private (firstName: String, lastName: String)
object FullName {
  def apply(firstName: String, lastName: String): FullName = {
    if (firstName.isEmpty || lastName.isEmpty) {
      throw new IllegalArgumentException("だめだよ")
    }
    new FullName(firstName, lastName)
  }
}

case class Money(amount: BigDecimal, currency: String) {
  def add(arg: Money): Money = {
    if (currency != arg.currency) {
      throw new IllegalArgumentException(
        s"通貨単位が異なります this: ${this.currency}, arg: ${arg.currency}"
      )
    }
    Money(amount + arg.amount, currency)
  }
}

case class ModelNumber(productCode: String, branch: String, lot: String) {
  override def toString: String = s"$productCode-$branch-$lot"
}
