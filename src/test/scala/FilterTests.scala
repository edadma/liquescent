//
package xyz.hyperreal.liquescent

import org.scalatest._
import prop.PropertyChecks


class FilterTests extends FreeSpec with PropertyChecks with Matchers with Testing {
	
	"abs" in {
		test(
			"""
				|{{ -17 | abs }}
				|{{ 4 | abs }}
				|{{ "-19.86" | abs }}
			""".stripMargin, false
		) shouldBe
			"""
				|17
				|4
				|19.86
			""".stripMargin
	}

	"append" in {
		test(
			"""
				|{{ "/my/fancy/url" | append: ".html" }}
				|{{ "website.com" | append: filename }}
			""".stripMargin, false, "filename" -> "/index.html"
		) shouldBe
			"""
				|/my/fancy/url.html
				|website.com/index.html
			""".stripMargin
	}

	"at_least" in {
		test(
			"""
				|{{ 4 | at_least: 5 }}
				|{{ 4 | at_least: 3 }}
			""".stripMargin, false
		) shouldBe
			"""
				|5
				|4
			""".stripMargin
	}

	"at_most" in {
		test(
			"""
				|{{ 4 | at_most: 5 }}
				|{{ 4 | at_most: 3 }}
			""".stripMargin, false
		) shouldBe
			"""
				|4
				|3
			""".stripMargin
	}

	"capitalize" in {
		test(
			"""
				|{{ "title" | capitalize }}
				|{{ "my great title" | capitalize }}
			""".stripMargin, false
		) shouldBe
			"""
				|Title
				|My great title
			""".stripMargin
	}

	"ceil" in {
		test(
			"""
				|{{ 1.2 | ceil }}
        |{{ -1.2 | ceil }}
				|{{ 2.0 | ceil }}
				|{{ 183.357 | ceil }}
				|{{ "3.5" | ceil }}
			""".stripMargin, false
		) shouldBe
			"""
				|2
        |-1
				|2
				|184
				|4
			""".stripMargin
	}

	"concat" in {
		test(
			"""
				|{% assign fruits = "apples, oranges, peaches" | split: ", " %}
				|{% assign vegetables = "carrots, turnips, potatoes" | split: ", " %}
				|
				|{% assign everything = fruits | concat: vegetables %}
				|
				|{% for item in everything %}
				|- {{ item }}
				|{% endfor %}
			""".stripMargin, false
		).trim shouldBe
			"""
				|- apples
				|
				|- oranges
				|
				|- peaches
				|
				|- carrots
				|
				|- turnips
				|
				|- potatoes
			""".trim.stripMargin
		test(
			"""
				|{% assign fruits = "apples, oranges, peaches" | split: ", " %}
				|{% assign vegetables = "carrots, turnips, potatoes" | split: ", " %}
				|{% assign furniture = "chairs, tables, shelves" | split: ", " %}
				|
				|{% assign everything = fruits | concat: vegetables | concat: furniture %}
				|
				|{% for item in everything %}
				|- {{ item }}
				|{% endfor %}
			""".stripMargin, false
		).trim shouldBe
			"""
				|- apples
				|
				|- oranges
				|
				|- peaches
				|
				|- carrots
				|
				|- turnips
				|
				|- potatoes
				|
				|- chairs
				|
				|- tables
				|
				|- shelves
			""".trim.stripMargin
	}

	"date" in {
		test(
			"""
				|{{ article.published_at | date: "%a, %b %d, %y" }}
				|{{ article.published_at | date: "%Y" }}
				|{{ "March 14, 2016" | date: "%b %d, %y" }}
			""".stripMargin, false, "article" -> Map("published_at" -> "2015-07-17")
		) shouldBe
			"""
				|Fri, Jul 17, 15
				|2015
				|Mar 14, 16
			""".stripMargin
	}

	"default" in {
		test(
			"""
				|{{ product_price | default: 2.99 }}
			""".stripMargin, true
		) shouldBe "2.99"
		test(
			"""
				|{% assign product_price = 4.99 %}
				|{{ product_price | default: 2.99 }}
			""".stripMargin, true
		) shouldBe "4.99"
		test(
			"""
				|{% assign product_price = "" %}
				|{{ product_price | default: 2.99 }}
			""".stripMargin, true
		) shouldBe "2.99"
	}

	"downcase" in {
		test(
			"""
				|{{ "Parker Moore" | downcase }}
				|{{ "apple" | downcase }}
			""".stripMargin, false
		) shouldBe
			"""
				|parker moore
				|apple
			""".stripMargin
	}

	"escape" in {
		test(
			"""
				|{{ "Have you read 'James & the Giant Peach'?" | escape }}
				|{{ "Tetsuro Takara" | escape }}
			""".stripMargin, false
		) shouldBe
			"""
				|Have you read &apos;James &amp; the Giant Peach&apos;?
				|Tetsuro Takara
			""".stripMargin
	}

	"escape_once" in {
		test(
			"""
				|{{ "1 < 2 & 3" | escape_once | escape_once }}
			""".stripMargin, false
		) shouldBe
			"""
				|1 &lt; 2 &amp; 3
			""".stripMargin
	}

	"first" in {
		test(
			"""
        |{% assign my_array = "apples, oranges, peaches, plums" | split: ", " %}
        |
        |{{ my_array.first }}
        |{{ my_array | first }}
			""".stripMargin, false
		).trim shouldBe
			"""
				|apples
				|apples
			""".trim.stripMargin
	}

  "floor" in {
    test(
      """
        |{{ 1.2 | floor }}
        |{{ -1.2 | floor }}
        |{{ 2.0 | floor }}
        |{{ 183.357 | floor }}
        |{{ "3.5" | floor }}
      """.stripMargin, false
    ) shouldBe
      """
        |1
        |-2
        |2
        |183
        |3
      """.stripMargin
  }

  "join" in {
    test(
      """
        |{% assign beatles = "John, Paul, George, Ringo" | split: ", " %}
        |
        |{{ beatles | join: " and " }}
      """.stripMargin, true
    ) shouldBe "John and Paul and George and Ringo"
  }

  "last" in {
    test(
      """
        |{% assign my_array = "apples, oranges, peaches, plums" | split: ", " %}
        |
        |{{ my_array.last }}
        |{{ my_array | last }}
      """.stripMargin, false
    ).trim shouldBe
      """
        |plums
        |plums
      """.trim.stripMargin
  }

  "lstrip" in {
    test(
      """
        |( {{ "          So much room for activities!          " | lstrip }} )
      """.stripMargin, false
    ).trim shouldBe "( So much room for activities!           )"
  }

  "minus" in {
    test(
      """
        |{{ 7 | minus: 3 }}
        |{{ 183.357 | minus: 12 }}
      """.stripMargin, true
    ) shouldBe "4 171.357"
  }

  "modulo" in {
    test(
      """
        |{{ 3 | modulo: 2 }}
        |{{ 24 | modulo: 7 }}
        |{{ 183.357 | modulo: 12 }}
      """.stripMargin, true
    ) shouldBe "1 3 3.357"
  }

  "newline_to_br" in {
    test(
      """
        |{% capture string_with_newlines %}
        |Hello
        |there
        |{% endcapture %}
        |
        |{{ string_with_newlines | newline_to_br }}
      """.stripMargin, false
    ).trim shouldBe "<br />Hello<br />there<br />"
  }

  "plus" in {
    test(
      """
        |{{ 4 | plus: 2 }}
        |{{ 183.357 | plus: 12 }}
      """.stripMargin, true
    ) shouldBe "6 195.357"
  }

  "prepend" in {
    test(
      """
        |{{ "apples, oranges, and bananas" | prepend: "Some fruit: " }}
      """.stripMargin, true
    ) shouldBe "Some fruit: apples, oranges, and bananas"
    test(
      """
        |{% assign url = "liquidmarkup.com" %}
        |
        |{{ "/index.html" | prepend: url }}
      """.stripMargin, true
    ) shouldBe "liquidmarkup.com/index.html"
  }

	"sort_natural" in {
		test( """{{ "zebra, octopus, giraffe, 2, 1, Sally Snake" | split: ", " | sort_natural | join: ", " }}""", false ) shouldBe
			"1, 2, giraffe, octopus, Sally Snake, zebra"
	}

  "split" in {
    test(
      """
        |{% assign beatles = "John, Paul, George, Ringo" | split: ", " %}
        |
        |{% for member in beatles %}
        |  {{ member }}
        |{% endfor %}
      """.stripMargin, false
    ).trim shouldBe
      """
        |John
        |
        |  Paul
        |
        |  George
        |
        |  Ringo
      """.trim.stripMargin
  }

  "times" in {
    test(
      """
        |{{ 3 | times: 2 }}
        |{{ 183.357 | times: 12 }}
      """.stripMargin, true
    ) shouldBe "6 2200.284"
  }

  "truncatewords" in {
    test(
      """
        |{{ "Ground control to Major Tom." | truncatewords: 3 }}
      """.stripMargin, true
    ) shouldBe "Ground control to..."
    test(
      """
        |{{ "Ground control to Major Tom." | truncatewords: 3, "--" }}
      """.stripMargin, true
    ) shouldBe "Ground control to--"
    test(
      """
        |{{ "Ground control to Major Tom." | truncatewords: 3, "" }}
      """.stripMargin, true
    ) shouldBe "Ground control to"
  }

  "uniq" in {
    test(
      """
        |{% assign my_array = "ants, bugs, bees, bugs, ants" | split: ", " %}
        |
        |{{ my_array | uniq | join: ", " }}
      """.stripMargin, false
    ).trim shouldBe "ants, bugs, bees"
  }

}