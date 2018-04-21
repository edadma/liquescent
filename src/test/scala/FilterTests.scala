package xyz.hyperreal.fluidic

import org.scalatest._
import prop.PropertyChecks


class FilterTests extends FreeSpec with PropertyChecks with Matchers with Testing {
	
	"abs" in {
		test(
			"""
				|{{ -17 | abs }}
				|{{ 4 | abs }}
				|{{ "-19.86" | abs }}
			""".stripMargin
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
			""".stripMargin
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
			""".stripMargin
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
			""".stripMargin
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
			""".stripMargin
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
				|{{ 2.0 | ceil }}
				|{{ 183.357 | ceil }}
				|{{ "3.5" | ceil }}
			""".stripMargin
		) shouldBe
			"""
				|2
				|2
				|184
				|4
			""".stripMargin
	}

	"date" in {
		test(
			"""
				|{{ 1.2 | ceil }}
				|{{ 2.0 | ceil }}
				|{{ 183.357 | ceil }}
				|{{ "3.5" | ceil }}
			""".stripMargin
		) shouldBe
			"""
				|2
				|2
				|184
				|4
			""".stripMargin
	}

	"downcase" in {
		test(
			"""
				|{{ "Parker Moore" | downcase }}
				|{{ "apple" | downcase }}
			""".stripMargin
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
			""".stripMargin
		) shouldBe
			"""
				|Have you read &apos;James &amp; the Giant Peach&apos;?
				|Tetsuro Takara
			""".stripMargin
	}

	"sort_natural" in {
		test( """{{ "zebra, octopus, giraffe, 2, 1, Sally Snake" | split: ", " | sort_natural | join: ", " }}""" ) shouldBe
			"1, 2, giraffe, octopus, Sally Snake, zebra"
	}

}