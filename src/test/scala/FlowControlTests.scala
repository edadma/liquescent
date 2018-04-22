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
				|blah
			""".stripMargin, "product" -> Map("title" -> "Awefull Shoes")
		).trim shouldBe
			"""
				|These shoes are not awesome.
				|
				|blah
			""".stripMargin.trim
		test(
			"""
				|{% unless product.title == 'Awesome Shoes' %}
				|  These shoes are not awesome.
				|{% endunless %}
				|blah
			""".stripMargin, "product" -> Map("title" -> "Awesome Shoes")
		).trim shouldBe
			"""
				|blah
			""".stripMargin.trim
	}

	"case" in {
		test(
			"""
				|{% assign handle = 'cake' %}
				|{% case handle %}
				|  {% when 'cake' %}
				|     This is a cake
				|  {% when 'cookie' %}
				|     This is a cookie
				|  {% else %}
				|     This is not a cake nor a cookie
				|{% endcase %}
			""".stripMargin
		).trim shouldBe
			"""
				|This is a cake
			""".stripMargin.trim
		test(
			"""
				|{% assign handle = 'cookie' %}
				|{% case handle %}
				|  {% when 'cake' %}
				|     This is a cake
				|  {% when 'cookie' %}
				|     This is a cookie
				|  {% else %}
				|     This is not a cake nor a cookie
				|{% endcase %}
			""".stripMargin
		).trim shouldBe
			"""
				|This is a cookie
			""".stripMargin.trim
		test(
			"""
				|{% assign handle = 'candy' %}
				|{% case handle %}
				|  {% when 'cake' %}
				|     This is a cake
				|  {% when 'cookie' %}
				|     This is a cookie
				|  {% else %}
				|     This is not a cake nor a cookie
				|{% endcase %}
			""".stripMargin
		).trim shouldBe
			"""
				|This is not a cake nor a cookie
			""".stripMargin.trim
	}

}