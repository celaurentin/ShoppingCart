package service

import model.Quantity.emptyQuantity
import model._

import scala.math.BigDecimal.double2bigDecimal

trait ShoppingCartService {

  def addProduct(
      shoppingCart: ShoppingCart,
      product: Product,
      quantity: Quantity
  ): ShoppingCart
  def removeProduct(
      shoppingCart: ShoppingCart,
      product: Product,
      quantity: Quantity
  ): Either[String, ShoppingCart]
  def updateTotalPrice(shoppingCart: ShoppingCart): ShoppingCart
  def calculateTax(shoppingCart: ShoppingCart): ShoppingCart

}

class ShoppingCartServiceImpl extends ShoppingCartService {

  override def addProduct(
      shoppingCart: ShoppingCart,
      product: Product,
      quantity: Quantity
  ): ShoppingCart =
    if (quantity == emptyQuantity) shoppingCart
    else {
      val currentQuantity = shoppingCart.items.getOrElse(product, emptyQuantity)
      val updatedQuantity = currentQuantity + quantity
      updateTotalPrice(
        shoppingCart.copy(items =
          shoppingCart.items.updated(product, updatedQuantity)
        )
      )
    }

  override def updateTotalPrice(shoppingCart: ShoppingCart): ShoppingCart = {
    val total = shoppingCart.items.foldLeft(BigDecimal(0.0))((acc, kv) =>
      acc + kv._1.price * BigDecimal(kv._2.value)
    )
    val shoppingCartWithTaxes = calculateTax(
      shoppingCart.copy(total = round(total))
    )
    val totalWithTaxes =
      shoppingCartWithTaxes.total + shoppingCartWithTaxes.taxes
    shoppingCartWithTaxes.copy(totalWithTaxes = round(totalWithTaxes))
  }

  override def calculateTax(shoppingCart: ShoppingCart): ShoppingCart = {
    val taxRate = 0.125
    val taxes = shoppingCart.total * taxRate
    shoppingCart.copy(taxes = round(taxes))
  }

  def round(value: BigDecimal): BigDecimal =
    value.setScale(2, BigDecimal.RoundingMode.HALF_EVEN)

  override def removeProduct(
      shoppingCart: ShoppingCart,
      product: Product,
      quantity: Quantity
  ): Either[String, ShoppingCart] = {
    if (quantity == emptyQuantity) Right(shoppingCart)
    else {
      val currentQuantity = shoppingCart.items.getOrElse(product, emptyQuantity)
      val updatedQuantity = currentQuantity.value - quantity.value
      if (updatedQuantity < 0)
        Left("ShoppingCart has not enough items to complete this operation")
      else if (updatedQuantity == 0) {
        Right(
          updateTotalPrice(
            shoppingCart.copy(items = shoppingCart.items.removed(product))
          )
        )
      } else
        Right(
          updateTotalPrice(
            shoppingCart.copy(items =
              shoppingCart.items.updated(product, Quantity(updatedQuantity))
            )
          )
        )
    }
  }
}
