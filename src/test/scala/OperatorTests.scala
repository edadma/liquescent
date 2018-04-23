//a
package xyz.hyperreal.liquescent

import org.scalatest._
import prop.PropertyChecks


class OperatorTests extends FreeSpec with PropertyChecks with Matchers with Testing {

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