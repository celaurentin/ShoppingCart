
import ShoppingCartSpec.Fixture
import model.{Product, Quantity, ShoppingCart}
import org.scalatest.OptionValues.convertOptionToValuable
import service._
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers


class ShoppingCartSpec extends AnyFunSpec with Matchers with Fixture{
    val shoppingCartService = new ShoppingCartServiceImpl

    describe("Create a Shopping Cart") {
        describe("empty shoppingCart") {
            it("object type should be [ShoppingCart]") {
                shoppingCart shouldBe a [ShoppingCart]
            }
            it("shoppingCart should be empty") {
                emptyShoppingCart.isEmpty shouldBe true
            }
        }

        describe("non-empty Shopping Cart") {
            it("should be non-empty") {
                shoppingCart.isEmpty shouldBe false
            }
        }
    }

    describe("Create a Product") {
        describe("empty Product") {
            it("object type should be [Product]") {
                product shouldBe a[Product]
            }
            it("product should be empty") {
                emptyProduct.isEmpty shouldBe true
            }
        }

        describe("Create a non-empty Product") {
            it("should be non-empty") {
                product.isEmpty shouldBe false
            }
        }
    }

    describe("Add products to an empty Shopping Cart") {
        describe("Add one product") {
            it("should contain one Product") {
                val expectedShoppingCart = shoppingCart
                val result = shoppingCartService.addProduct(emptyShoppingCart, product, Quantity(1))
                result shouldBe expectedShoppingCart
                result.items.size == 1
            }
        }

        describe("Add one product with quantity equals 0") {
            it("should be empty") {
                val result = shoppingCartService.addProduct(emptyShoppingCart, product, Quantity(0))
                result shouldBe emptyShoppingCart
                result.items.isEmpty
            }
        }

        describe("Add two different products") {
            it("should contain two products") {
                val result = shoppingCartService
                  .addProduct(
                    shoppingCartService.addProduct(emptyShoppingCart, product, Quantity(1)),
                    product.copy(2, "Ivory"),
                      Quantity(1)
                  )
                result shouldBe shoppingCartWithTwoProducts
                result.items.size == 2
            }
        }
    }

    describe("Step 1 - Add five products to an empty Shopping Cart") {
        it("should contain five Products") {
            val result = shoppingCartService.addProduct(emptyShoppingCart, product, Quantity(5))
            result shouldBe shoppingCartWithFiveDoveSoaps
            result.items.size shouldEqual 1
            val quantity = result.items.get(product)
            quantity.value shouldBe Quantity(5)
            result.total shouldBe 199.95
        }
    }

}

object ShoppingCartSpec {
    trait Fixture {
        val product = Product(productId = 1, name = "Dove Soap", price =39.99)

        val emptyProduct = product.copy(name = "")

        val items = Map(product -> Quantity(1))

        val shoppingCart = ShoppingCart(shoppingCartId = 1, items = items, total = 39.99, tax = 0.0)

        val emptyShoppingCart = shoppingCart.copy(items = Map.empty, total = 0.0, tax = 0.0)

        val shoppingCartWithTwoProducts =
            shoppingCart.copy(items = Map(product -> Quantity(1), product.copy(productId = 2, name = "Ivory") -> Quantity(1)), total = 79.98, tax = 0.0)

        val shoppingCartWithFiveDoveSoaps = shoppingCart.copy(items = Map(product -> Quantity(5)), total = 199.95, tax = 0.0)


    }
}
