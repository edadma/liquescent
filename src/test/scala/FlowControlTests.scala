package xyz.hyperreal.liquescent

import org.scalatest._
import prop.PropertyChecks


class FlowControlTests extends FreeSpec with PropertyChecks with Matchers with Testing {

	"unless" in {
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

}