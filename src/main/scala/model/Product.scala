package model

case class Product(
  productId: Integer,
  name: String,
  price: Double
) {
  require(productId > 0, "productId must be greater than 0")

  def isEmpty: Boolean = name.trim.isEmpty
}
