package model

case class ShoppingCart(
    shoppingCartId: Integer,
    items: Map[Product, Quantity],
    total: BigDecimal,
    totalWithTaxes: BigDecimal,
    taxes: BigDecimal
) {
  require(shoppingCartId > 0, "shoppingCartId must be greater than 0")

  def isEmpty: Boolean = items.isEmpty
}
