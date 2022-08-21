package model

case class ShoppingCart(
 shoppingCartId: Integer,
 items: Map[Product, Quantity],
 total: BigDecimal,
 tax: BigDecimal
) {
  require(shoppingCartId > 0, "shoppingCartId must be greater than 0")

  def isEmpty: Boolean = items.isEmpty
}


case class Quantity(
  value: Integer
) {
  require(value >=0 , "quantity must be zero or greater than zero")

}
