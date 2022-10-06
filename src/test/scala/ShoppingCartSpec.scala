import ShoppingCartSpec.Fixture
import model.{Product, Quantity, ShoppingCart}
import org.scalatest.EitherValues
import org.scalatest.OptionValues.convertOptionToValuable
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import service._

class ShoppingCartSpec
    extends AnyFunSpec
    with Matchers
    with EitherValues
    with Fixture {

  val shoppingCartService = new ShoppingCartServiceImpl

  describe("Create a Shopping Cart") {
    describe("empty shoppingCart") {
      it("object type should be [ShoppingCart]") {
        emptyShoppingCart shouldBe a[ShoppingCart]
      }
      it("shoppingCart should be empty") {
        emptyShoppingCart.isEmpty shouldBe true
      }
    }

    describe("non-empty Shopping Cart") {
      it("should be non-empty") {
        doveShoppingCart.isEmpty shouldBe false
      }
    }
  }

  describe("Create a Product") {
    describe("empty Product") {
      it("object type should be [Product]") {
        doveProduct shouldBe a[Product]
      }
      it("product should be empty") {
        emptyProduct.isEmpty shouldBe true
      }
    }

    describe("Create a non-empty Product") {
      it("should be non-empty") {
        doveProduct.isEmpty shouldBe false
      }
    }
  }

  describe("Add products to an empty Shopping Cart") {
    describe("Add one product") {
      it("should contain one Product") {
        val result = shoppingCartService.addProduct(
          emptyShoppingCart,
          doveProduct,
          Quantity(1)
        )
        result shouldBe doveShoppingCart
        result.items.size shouldEqual 1
      }
    }

    describe("Add one product with quantity equals 0") {
      it("should be empty") {
        val result = shoppingCartService.addProduct(
          emptyShoppingCart,
          doveProduct,
          Quantity(0)
        )
        result shouldBe emptyShoppingCart
        result.items.isEmpty
      }
    }

    describe("Add two different products") {
      it("should contain two products") {
        val result = shoppingCartService
          .addProduct(
            shoppingCartService
              .addProduct(emptyShoppingCart, doveProduct, Quantity(1)),
            ivoryProduct,
            Quantity(1)
          )
        result.items.size shouldEqual 2
        result shouldBe shoppingCartWithTwoProducts
      }
    }
  }

  describe("Step 1 - Add five products to an empty Shopping Cart") {
    it("should contain five Products") {
      val result = shoppingCartService.addProduct(
        emptyShoppingCart,
        doveProduct,
        Quantity(5)
      )
      result.items.size shouldEqual 1
      val quantity = result.items.get(doveProduct)
      quantity.value shouldEqual Quantity(5)
      result.total shouldEqual 199.95
      result.taxes shouldEqual 24.99
      result.totalWithTaxes shouldEqual 224.94
      result shouldBe shoppingCartWithFiveDoveSoaps
    }
  }

  describe(
    "Step 2 - Add additional products to an empty Shopping Cart (5 plus 3)"
  ) {
    it("should contain eight Products") {
      val partialResult = shoppingCartService.addProduct(
        emptyShoppingCart,
        doveProduct,
        Quantity(5)
      )
      val result =
        shoppingCartService.addProduct(partialResult, doveProduct, Quantity(3))
      result.items.size shouldEqual 1
      val quantity = result.items.get(doveProduct)
      quantity.value shouldEqual Quantity(8)
      result.total shouldEqual 319.92
      result.totalWithTaxes shouldEqual 359.91
      result.taxes shouldEqual 39.99
      result shouldBe shoppingCartWithEightDoveSoaps
    }
  }

  describe("Step 3 - Calculate the tax rate") {
    describe("calculate the tax rate for a shopping cart with one product") {
      it("should contain one Product and valid tax") {
        val result = shoppingCartService.calculateTax(doveShoppingCart)
        val quantity = result.items.get(doveProduct)
        quantity.value shouldEqual Quantity(1)
        result.taxes shouldEqual 5.0
      }
    }
    describe(
      "calculate the tax rate for a shopping cart with multiple products"
    ) {
      it("should contain 4 Products and valid tax") {
        val partialResult = shoppingCartService.addProduct(
          emptyShoppingCart,
          doveProduct,
          Quantity(2)
        )
        val result =
          shoppingCartService.addProduct(partialResult, axeProduct, Quantity(2))
        val doveQuantity = result.items.get(doveProduct)
        val axeQuantity = result.items.get(axeProduct)
        doveQuantity.value shouldEqual Quantity(2)
        axeQuantity.value shouldEqual Quantity(2)
        result.taxes shouldEqual 35.00
        result.totalWithTaxes shouldEqual 314.96
        result shouldBe shoppingCartWithTwoDoveAndTwoAxe
      }
    }
  }

  describe("Step 4 - Remove product") {
    describe("Remove zero products") {
      it("should be the original amount of products") {
        val result = shoppingCartService.removeProduct(
          shoppingCartWithTwoProducts,
          doveProduct,
          Quantity(0)
        )
        result shouldBe Right(shoppingCartWithTwoProducts)
        result.value.items.size shouldEqual 2
      }
    }
    describe("Remove one product from an empty shopping cart") {
      it("should return an error message") {
        val result = shoppingCartService.removeProduct(
          emptyShoppingCart,
          doveProduct,
          Quantity(1)
        )
        result shouldBe Left(
          "ShoppingCart has not enough items to complete this operation"
        )
      }
    }
    describe("Remove one product from a shopping cart") {
      it("should return an empty shopping cart") {
        val result = shoppingCartService.removeProduct(
          doveShoppingCart,
          doveProduct,
          Quantity(1)
        )
        result shouldBe Right(
          emptyShoppingCart
        )
      }
    }
    describe("Remove more products than available in shopping cart") {
      it("should return an error message") {
        val result = shoppingCartService.removeProduct(
          shoppingCartWithFiveDoveSoaps,
          doveProduct,
          Quantity(10)
        )
        result shouldBe Left(
          "ShoppingCart has not enough items to complete this operation"
        )
      }
    }
    describe("Remove one product from a two products shopping cart") {
      it("should return a shopping cart with one product") {
        val result = shoppingCartService.removeProduct(
          shoppingCartWithTwoProducts,
          ivoryProduct,
          Quantity(1)
        )
        result shouldBe Right(doveShoppingCart)
        result.value.items.size shouldEqual 1
      }
    }
  }
}

object ShoppingCartSpec {
  trait Fixture {
    val doveProduct = Product(productId = 1, name = "Dove Soap", price = 39.99)

    val ivoryProduct =
      Product(productId = 2, name = "Ivory Soap", price = 39.99)

    val axeProduct = Product(productId = 3, name = "Axe Deo", price = 99.99)

    val emptyProduct = doveProduct.copy(name = "")

    val items = Map(doveProduct -> Quantity(1))

    val doveShoppingCart = ShoppingCart(
      shoppingCartId = 1,
      items = items,
      total = 39.99,
      totalWithTaxes = 44.99,
      taxes = 5.0
    )

    val ivoryShoppingCart = ShoppingCart(
      shoppingCartId = 2,
      items = Map(
        ivoryProduct -> Quantity(1)
      ),
      total = 39.99,
      totalWithTaxes = 44.99,
      taxes = 5.0
    )

    val emptyShoppingCart =
      doveShoppingCart.copy(
        items = Map.empty,
        total = 0.0,
        totalWithTaxes = 0.0,
        taxes = 0.0
      )

    val shoppingCartWithTwoProducts = ShoppingCart(
      shoppingCartId = 1,
      items = Map(
        doveProduct -> Quantity(1),
        ivoryProduct -> Quantity(1)
      ),
      total = 79.98,
      totalWithTaxes = 89.98,
      taxes = 10.0
    )

    val shoppingCartWithFiveDoveSoaps = doveShoppingCart.copy(
      items = Map(doveProduct -> Quantity(5)),
      total = 199.95,
      totalWithTaxes = 224.94,
      taxes = 24.99
    )

    val shoppingCartWithEightDoveSoaps = doveShoppingCart.copy(
      items = Map(doveProduct -> Quantity(8)),
      total = 319.92,
      totalWithTaxes = 359.91,
      taxes = 39.99
    )

    val shoppingCartWithTwoDoveAndTwoAxe = doveShoppingCart.copy(
      items = Map(doveProduct -> Quantity(2), axeProduct -> Quantity(2)),
      total = 279.96,
      totalWithTaxes = 314.96,
      taxes = 35.0
    )

  }
}
