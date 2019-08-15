//@
package xyz.hyperreal.liquescent

import org.scalatest._
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks


class IterationTests extends FreeSpec with ScalaCheckPropertyChecks with Matchers with Testing {

	"for" in {
		test(
			"""
				|{% for product in collection.products %}
				|  {{ product.title }}
				|{% endfor %}
			""".stripMargin, true, "collection" -> Map("products" -> List(Map("title" -> "hat"), Map("title" -> "shirt"), Map("title" -> "pants")))
		) shouldBe "hat shirt pants"
	}

	"break" in {
		test(
			"""
				|{% for i in (1..5) %}
				|  {% if i == 4 %}
				|    {% break %}
				|  {% else %}
				|    {{ i }}
				|  {% endif %}
				|{% endfor %}
			""".stripMargin, true
		) shouldBe "1 2 3"
	}

	"continue" in {
		test(
			"""
				|{% for i in (1..5) %}
				|  {% if i == 4 %}
				|    {% continue %}
				|  {% else %}
				|    {{ i }}
				|  {% endif %}
				|{% endfor %}
			""".stripMargin, true
		) shouldBe "1 2 3 5"
	}

	"limit" in {
		test(
			"""
				|{% for item in array limit:2 %}
				|  {{ item }}
				|{% endfor %}
			""".stripMargin, true, "array" -> List( 1, 2, 3, 4, 5, 6 )
		) shouldBe "1 2"
	}

	"offset" in {
		test(
			"""
				|{% for item in array offset:2 %}
				|  {{ item }}
				|{% endfor %}
			""".stripMargin, true, "array" -> List( 1, 2, 3, 4, 5, 6 )
		) shouldBe "3 4 5 6"
	}

	"range" in {
		test(
			"""
				|{% for i in (3..5) %}
 				|  {{ i }}
				|{% endfor %}
			""".stripMargin, true
		) shouldBe "3 4 5"
		test(
			"""
				|{% assign num = 4 %}
				|{% for i in (1..num) %}
 				|  {{ i }}
				|{% endfor %}
			""".stripMargin, true
		) shouldBe "1 2 3 4"
	}

	"reversed" in {
		test(
			"""
				|{% for item in array reversed %}
				|  {{ item }}
				|{% endfor %}
			""".stripMargin, true, "array" -> List( 1, 2, 3, 4, 5, 6 )
		) shouldBe "6 5 4 3 2 1"
	}

	"cycle" in {
		test(
			"""
				|{% for item in (3..6) %}
				|  {% cycle 'one', 'two', 'three' %}
				|{% endfor %}
			""".stripMargin, true
		) shouldBe "one two three one"
	}

}