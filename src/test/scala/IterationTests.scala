package xyz.hyperreal.liquescent

import org.scalatest._
import prop.PropertyChecks


class IterationTests extends FreeSpec with PropertyChecks with Matchers with Testing {

	"for" in {
		test(
			"""
				|{% for product in collection.products %}
				|  {{ product.title }}
				|{% endfor %}
			""".stripMargin, true, "collection" -> Map("products" -> List(Map("title" -> "hat"), Map("title" -> "shirt"), Map("title" -> "pants")))
		) shouldBe "hat shirt pants"
		test(
			"""
				|{% unless product.title == 'Awesome Shoes' %}
				|  These shoes are not awesome.
				|{% endunless %}
				|blah
			""".stripMargin, false, "product" -> Map("title" -> "Awesome Shoes")
		).trim shouldBe
			"""
				|blah
			""".stripMargin.trim
	}

}