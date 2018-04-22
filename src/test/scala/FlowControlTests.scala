package xyz.hyperreal.liquescent

import org.scalatest._
import prop.PropertyChecks


class FlowControlTests extends FreeSpec with PropertyChecks with Matchers with Testing {

	"unless" in {
		test(
			"""
				|{% unless product.title == 'Awesome Shoes' %}
				|  These shoes are not awesome.
				|{% endunless %}
			""".stripMargin, "product" -> Map("title" -> "Awefull Shoes")
		).trim shouldBe
			"""
				|These shoes are not awesome.
			""".stripMargin.trim
	}

}