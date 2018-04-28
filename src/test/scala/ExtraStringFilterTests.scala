//@
package xyz.hyperreal.liquescent

import org.scalatest._
import prop.PropertyChecks


class ExtraStringFilterTests extends FreeSpec with PropertyChecks with Matchers with Testing {

	"handleize" in {
		test(
			"""
				|{{ '100% M & Ms!!!' | handleize }}
			""".stripMargin, false
		).trim shouldBe
			"""
				|100-m-ms
			""".trim.stripMargin
	}

}