//a
package xyz.hyperreal.liquescent

import org.scalatest._
import prop.PropertyChecks


class OperatorTests extends FreeSpec with PropertyChecks with Matchers with Testing {

  "connectives" in {
    test(
      """
        |{% if product.type == "Shirt" or product.type == "Shoes" %}
        |  This is a shirt or a pair of shoes.
        |{% else %}
        |  It's neither a shirt nor a pair of shoes.
        |{% endif %}
      """.stripMargin, true, "product" -> Map("type" -> "Shoes")
    ) shouldBe "This is a shirt or a pair of shoes."
    test(
      """
        |{% if product.type == "Shirt" or product.type == "Shoes" %}
        |  This is a shirt or a pair of shoes.
        |{% else %}
        |  It's neither a shirt nor a pair of shoes.
        |{% endif %}
      """.stripMargin, true, "product" -> Map("type" -> "Toothbrush")
    ) shouldBe "It's neither a shirt nor a pair of shoes."
    test(
      """
        |{% if product.type == "Shirt" and product.color == "Blue" %}
        |  This is a blue shirt.
        |{% else %}
        |  It's nor a blue shirt.
        |{% endif %}
      """.stripMargin, true, "product" -> Map("type" -> "Shirt", "color" -> "Blue")
    ) shouldBe "This is a blue shirt."
    test(
      """
        |{% if product.type == "Shirt" and product.color == "Blue" %}
        |  This is a blue shirt.
        |{% else %}
        |  It's nor a blue shirt.
        |{% endif %}
      """.stripMargin, true, "product" -> Map("type" -> "Pants", "color" -> "Blue")
    ) shouldBe "It's nor a blue shirt."
  }

	"contains" in {
		test(
			"""
				|{% if product.title contains 'Pack' %}
				|  This product's title contains the word Pack.
				|{% endif %}
			""".stripMargin, true, "product" -> Map("title" -> "Packs and bags")
		) shouldBe "This product's title contains the word Pack."
		test(
			"""
				|{% if product.title contains 'asdf' %}
				|  This product's title contains the word asdf.
				|{% else %}
				|  no asdf
				|{% endif %}
			""".stripMargin, true, "product" -> Map("title" -> "Packs and bags")
		) shouldBe "no asdf"
		test(
			"""
				|{% if product.tags contains 'Hello' %}
				|  This product has been tagged with 'Hello'.
				|{% endif %}
			""".stripMargin, true, "product" -> Map("tags" -> List("wow", "Hello", "lala"))
		) shouldBe "This product has been tagged with 'Hello'."
	}

}