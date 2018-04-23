//a
package xyz.hyperreal.liquescent

import org.scalatest._
import prop.PropertyChecks


class VariableTests extends FreeSpec with PropertyChecks with Matchers with Testing {

	"assign" in {
		test(
			"""
				|{% assign my_variable = false %}
				|{% if my_variable != true %}
				|  This statement is valid.
				|{% endif %}
			""".stripMargin, true
		) shouldBe "This statement is valid."
		test(
			"""
				|{% assign foo = "bar" %}
				|{{ foo }}
			""".stripMargin, true
		) shouldBe "bar"
	}

	"capture" in {
		test(
			"""
				|{% capture my_variable %}I am being captured.{% endcapture %}
				|{{ my_variable }}
			""".stripMargin, true
		) shouldBe "I am being captured."
		test(
			"""
				|{% assign favorite_food = 'pizza' %}
				|{% assign age = 35 %}
				|
				|{% capture about_me %}
				|I am {{ age }} and my favorite food is {{ favorite_food }}.
				|{% endcapture %}
				|
				|{{ about_me }}
			""".stripMargin, true
		) shouldBe "I am 35 and my favorite food is pizza."
	}

	"increment" in {
		test(
			"""
				|{% increment my_counter %}
				|{% increment my_counter %}
				|{% increment my_counter %}
			""".stripMargin, false
		) shouldBe
			"""
				|0
				|1
				|2
			""".stripMargin
		test(
			"""
				|{% assign var = 10 %}
				|{% increment var %}
				|{% increment var %}
				|{% increment var %}
				|{{ var }}
			""".stripMargin, true
		) shouldBe "0 1 2 10"
	}

	"decrement" in {
		test(
			"""
				|{% decrement variable %}
				|{% decrement variable %}
				|{% decrement variable %}
			""".stripMargin, false
		) shouldBe
			"""
				|-1
				|-2
				|-3
			""".stripMargin
	}

}