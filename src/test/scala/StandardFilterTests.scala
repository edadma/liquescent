//@
package xyz.hyperreal.liquescent

import org.scalatest._
import prop.PropertyChecks


class StandardFilterTests extends FreeSpec with PropertyChecks with Matchers with Testing {
	
	"abs" in {
		test(
			"""
				|{{ -17 | abs }}
				|{{ 4 | abs }}
				|{{ "-19.86" | abs }}
			""".trim.stripMargin, false
		) shouldBe
			"""
				|17
				|4
				|19.86
			""".trim.stripMargin
	}

	"append" in {
		test(
			"""
				|{{ "/my/fancy/url" | append: ".html" }}
				|{{ "website.com" | append: filename }}
			""".trim.stripMargin, false, "filename" -> "/index.html"
		) shouldBe
			"""
				|/my/fancy/url.html
				|website.com/index.html
			""".trim.stripMargin
	}

	"at_least" in {
		test(
			"""
				|{{ 4 | at_least: 5 }}
				|{{ 4 | at_least: 3 }}
			""".trim.stripMargin, false
		) shouldBe
			"""
				|5
				|4
			""".trim.stripMargin
	}

	"at_most" in {
		test(
			"""
				|{{ 4 | at_most: 5 }}
				|{{ 4 | at_most: 3 }}
			""".trim.stripMargin, false
		) shouldBe
			"""
				|4
				|3
			""".trim.stripMargin
	}

	"capitalize" in {
		test(
			"""
				|{{ "title" | capitalize }}
				|{{ "my great title" | capitalize }}
			""".trim.stripMargin, false
		) shouldBe
			"""
				|Title
				|My great title
			""".trim.stripMargin
	}

	"ceil" in {
		test(
			"""
				|{{ 1.2 | ceil }}
        |{{ -1.2 | ceil }}
				|{{ 2.0 | ceil }}
				|{{ 183.357 | ceil }}
				|{{ "3.5" | ceil }}
			""".trim.stripMargin, false
		) shouldBe
			"""
				|2
        |-1
				|2
				|184
				|4
			""".trim.stripMargin
	}

  "compact" in {
    test(
      """
        |{% assign site_categories = site.pages | map: 'category' | compact %}
        |
        |{% for category in site_categories %}
        |  {{ category }}
        |{% endfor %}
      """.stripMargin, false, "site" -> Map( "pages" -> List(
        Map("category" -> "business"),
        Map("category" -> "celebrities"),
        Map("asdf" -> "asdf"),
        Map("category" -> "lifestyle"),
        Map("category" -> "sports"),
        Map("asdf" -> "asdf"),
        Map("category" -> "technology")) )
    ).trim shouldBe
      """
        |business
        |
        |  celebrities
        |
        |  lifestyle
        |
        |  sports
        |
        |  technology
      """.trim.stripMargin
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
			""".trim.stripMargin, false, "article" -> Map("published_at" -> "2015-07-17")
		) shouldBe
			"""
				|Fri, Jul 17, 15
				|2015
				|Mar 14, 16
			""".trim.stripMargin
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

  "divided_by" in {
    test(
      """
        |{{ 16 | divided_by: 4 }}
        |{{ 5 | divided_by: 3 }}
        |{{ 20 | divided_by: 7 }}
        |{{ 20 | divided_by: 7.0 }}
        |
        |{% assign my_integer = 7 %}
        |{{ 20 | divided_by: my_integer }}
        |
        |{% assign my_integer = 7 %}
        |{% assign my_float = my_integer | times: 1.0 %}
        |{{ 20 | divided_by: my_float }}
      """.stripMargin, true
    ) shouldBe "4 1 2 2.857142857142857 2 2.857142857142857"
  }

	"downcase" in {
		test(
			"""
				|{{ "Parker Moore" | downcase }}
				|{{ "apple" | downcase }}
			""".trim.stripMargin, false
		) shouldBe
			"""
				|parker moore
				|apple
			""".trim.stripMargin
	}

	"escape" in {
		test(
			"""
				|{{ "Have you read 'James & the Giant Peach'?" | escape }}
				|{{ "Tetsuro Takara" | escape }}
			""".trim.stripMargin, false
		) shouldBe
			"""
				|Have you read &apos;James &amp; the Giant Peach&apos;?
				|Tetsuro Takara
			""".trim.stripMargin
	}

	"escape_once" in {
		test(
			"""
				|{{ "1 < 2 & 3" | escape_once | escape_once }}
			""".trim.stripMargin, false
		) shouldBe
			"""
				|1 &lt; 2 &amp; 3
			""".trim.stripMargin
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
      """.trim.stripMargin, false
    ) shouldBe
      """
        |1
        |-2
        |2
        |183
        |3
      """.trim.stripMargin
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

  "map" in {
    test(
      """
        |{% assign all_categories = site.pages | map: "category" %}
        |
        |{% for item in all_categories %}
        |{{ item }}
        |{% endfor %}
      """.stripMargin, false, "site" -> Map( "pages" -> List(
        Map("category" -> "business"),
        Map("category" -> "celebrities"),
        Map("category" -> "lifestyle"),
        Map("category" -> "sports"),
        Map("category" -> "technology")) )
    ).trim shouldBe
      """
        |business
        |
        |celebrities
        |
        |lifestyle
        |
        |sports
        |
        |technology
      """.trim.stripMargin
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

  "remove" in {
    test(
      """
        |{{ "I strained to see the train through the rain" | remove: "rain" }}
      """.stripMargin, true
    ) shouldBe "I sted to see the t through the"
  }

  "remove_first" in {
    test(
      """
        |{{ "I strained to see the train through the rain" | remove_first: "rain" }}
      """.stripMargin, true
    ) shouldBe "I sted to see the train through the rain"
  }

  "replace" in {
    test(
      """
        |{{ "Take my protein pills and put my helmet on" | replace: "my", "your" }}
      """.stripMargin, true
    ) shouldBe "Take your protein pills and put your helmet on"
  }

  "replace_first" in {
    test(
      """
        |{% assign my_string = "Take my protein pills and put my helmet on" %}
        |{{ my_string | replace_first: "my", "your" }}
      """.stripMargin, true
    ) shouldBe "Take your protein pills and put my helmet on"
  }

  "reverse" in {
    test(
      """
        |{% assign my_array = "apples, oranges, peaches, plums" | split: ", " %}
        |
        |{{ my_array | reverse | join: ", " }}
      """.stripMargin, true
    ) shouldBe "plums, peaches, oranges, apples"
    test(
      """
        |{{ "Ground control to Major Tom." | split: "" | reverse | join: "" }}
      """.stripMargin, true
    ) shouldBe ".moT rojaM ot lortnoc dnuorG"
  }

  "round" in {
    test(
      """
        |{{ 1.2 | round }}
        |{{ 2.7 | round }}
        |{{ 183.357 | round: 2 }}
      """.stripMargin, true
    ) shouldBe "1 3 183.36"
  }

  "rstrip" in {
    test(
      """
        |( {{ "          So much room for activities!          " | rstrip }} )
      """.stripMargin, false
    ).trim shouldBe "(           So much room for activities! )"
  }

  "size" in {
    test(
      """
        |{{ "Ground control to Major Tom." | size }}
      """.stripMargin, true
    ) shouldBe "28"
    test(
      """
        |{% assign my_array = "apples, oranges, peaches, plums" | split: ", " %}
        |
        |{{ my_array | size }}
      """.stripMargin, true
    ) shouldBe "4"
    test(
      """
        |{% if site.pages.size > 10 %}
        |  This is a big website!
        |{% endif %}
      """.stripMargin, true, "site" -> Map("pages" -> Map("size" -> 12))
    ) shouldBe "This is a big website!"
  }

  "slice" in {
    test(
      """
        |{{ "Liquid" | slice: 2 }}
      """.stripMargin, true
    ) shouldBe "q"
    test(
      """
        |{{ "Liquid" | slice: 2, 5 }}
      """.stripMargin, true
    ) shouldBe "quid"
    test(
      """
        |{{ "Liquid" | slice: -3, 2 }}
      """.stripMargin, true
    ) shouldBe "ui"
  }

  "sort" in {
    test(
      """{{ "zebra, octopus, giraffe, 2, 1, Sally Snake" | split: ", " | sort | join: ", " }}""", false
    ) shouldBe "1, 2, Sally Snake, giraffe, octopus, zebra"
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

  "strip" in {
    test(
      """
        |( {{ "          So much room for activities!          " | strip }} )
      """.stripMargin, false
    ).trim shouldBe "( So much room for activities! )"
  }

  "strip_html" in {
    test(
      """
        |{{ "Have <em>you</em> read <strong>Ulysses</strong>?" | strip_html }}
      """.stripMargin, false
    ).trim shouldBe "Have you read Ulysses?"
  }

  "strip_newlines" in {
    test(
      """
        |{% capture string_with_newlines %}
        |Hello
        |there
        |{% endcapture %}
        |
        |{{ string_with_newlines | strip_newlines }}
      """.stripMargin, false
    ).trim shouldBe "Hellothere"
  }

  "times" in {
    test(
      """
        |{{ 3 | times: 2 }}
        |{{ 183.357 | times: 12 }}
      """.stripMargin, true
    ) shouldBe "6 2200.284"
  }

  "truncate" in {
    test(
      """
        |{{ "Ground control to Major Tom." | truncate: 20 }}
      """.stripMargin, true
    ) shouldBe "Ground control to..."
    test(
      """
        |{{ "Ground control to Major Tom." | truncate: 25, ", and so on" }}
      """.stripMargin, true
    ) shouldBe "Ground control, and so on"
    test(
      """
        |{{ "Ground control to Major Tom." | truncate: 20, "" }}
      """.stripMargin, true
    ) shouldBe "Ground control to Ma"
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