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

	"sort_natural" in {
		test( """{{ "zebra, octopus, giraffe, 2, 1, Sally Snake" | split: ", " | sort_natural | join: ", " }}""" ) shouldBe
			"1, 2, giraffe, octopus, Sally Snake, zebra"
	}

}