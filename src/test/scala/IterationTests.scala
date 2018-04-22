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
				|{% for i in (1..5) %}
				|  {% if i == 4 %}
				|    {% break %}
				|  {% else %}
				|    {{ i }}
				|  {% endif %}
				|{% endfor %}
			""".stripMargin, true
		) shouldBe "1 2 3"
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

}