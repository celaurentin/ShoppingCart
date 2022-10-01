package model

case class Quantity(
    value: Integer
) {
  require(value >= 0, "quantity must be zero or greater than zero")
  def +(that: Quantity): Quantity =
    Quantity(this.value + that.value)

}
object Quantity {
  val emptyQuantity: Quantity = Quantity(0)
}
