package service

import model.Quantity.emptyQuantity
import model._

import scala.math.BigDecimal.double2bigDecimal

trait ShoppingCartService {

  def addProduct(shoppingCart: ShoppingCart, product: Product, quantity: Quantity): ShoppingCart
  def updateTotalPrice(shoppingCart: ShoppingCart): ShoppingCart

}

class ShoppingCartServiceImpl extends ShoppingCartService {

  override def addProduct(shoppingCart: ShoppingCart, product: Product, quantity: Quantity): ShoppingCart =
    if (quantity.value==0) shoppingCart
    else {
      val currentQuantity = shoppingCart.items.getOrElse(product, emptyQuantity)
      val updatedQuantity = Quantity(currentQuantity.value + quantity.value)
      updateTotalPrice(shoppingCart.copy(items = shoppingCart.items.updated(product, updatedQuantity)))
    }

  override def updateTotalPrice(shoppingCart: ShoppingCart): ShoppingCart = {
    val total = shoppingCart.items.foldLeft(0.0)((acc, kv) => acc + kv._1.price * kv._2.value)
    shoppingCart.copy(total = total.setScale(2, BigDecimal.RoundingMode.HALF_EVEN))
  }
}