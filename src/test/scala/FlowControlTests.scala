package xyz.hyperreal.liquescent

import org.scalatest._
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks


class FlowControlTests extends FreeSpec with ScalaCheckPropertyChecks with Matchers with Testing {

  "if" in {
    test(
      """
        |{% if product.title == 'Awesome Shoes' %}
        |  These shoes are awesome.
        |{% endif %}
        |blah
      """.stripMargin, false, "product" -> Map("title" -> "Awesome Shoes")
    ).trim shouldBe
      """
        |These shoes are awesome.
        |
        |blah
      """.stripMargin.trim
    test(
      """
        |{% if product.title == 'Awesome Shoes' %}
        |  These shoes are awesome.
        |{% endif %}
        |blah
      """.stripMargin, false, "product" -> Map("title" -> "Awefull Shoes")
    ).trim shouldBe
      """
        |blah
      """.stripMargin.trim
  }

	"unless" in {
		test(
			"""
				|{% unless product.title == 'Awesome Shoes' %}
				|  These shoes are not awesome.
				|{% endunless %}
				|blah
			""".stripMargin, false, "product" -> Map("title" -> "Awefull Shoes")
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
			""".stripMargin, false, "product" -> Map("title" -> "Awesome Shoes")
		).trim shouldBe
			"""
				|blah
			""".stripMargin.trim
	}

  "elsif" in {
    test(
      """
        |{% if customer.name == 'kevin' %}
        |  Hey Kevin!
        |{% elsif customer.name == 'anonymous' %}
        |  Hey Anonymous!
        |{% else %}
        |  Hi Stranger!
        |{% endif %}
      """.stripMargin, true, "customer" -> Map("name" -> "anonymous")
    ).trim shouldBe "Hey Anonymous!"
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
			""".stripMargin, false
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
			""".stripMargin, false
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
			""".stripMargin, false
		).trim shouldBe
			"""
				|This is not a cake nor a cookie
			""".stripMargin.trim
	}

}