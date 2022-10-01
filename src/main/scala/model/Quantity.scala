package model

case class Quantity(
    value: Integer
) {
  require(value >= 0, "quantity must be zero or greater than zero")

}
object Quantity {
  val emptyQuantity: Quantity = Quantity(0)
}
