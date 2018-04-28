//@
package xyz.hyperreal.liquescent

import org.scalatest._
import prop.PropertyChecks


class ExtraStringFilterTests extends FreeSpec with PropertyChecks with Matchers with Testing {

	"camelcase" in {
		test(
			"""
				|{{ 'coming-soon' | camelcase }}
			""".stripMargin, false
		).trim shouldBe
			"""
				|ComingSoon
			""".trim.stripMargin
	}

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

	"md5" in {
		test(
			"""
				|{{ 'This is a test' | md5 }}
			""".stripMargin, false
		).trim shouldBe
			"""
				|CE114E4501D2F4E2DCEA3E17B546F339
			""".trim.stripMargin
	}

	"sha1" in {
		test(
			"""
				|{{ 'This is a test' | sha1 }}
			""".stripMargin, false
		).trim shouldBe
			"""
				|a54d88e06612d820bc3be72877c74f257b561b19
			""".trim.stripMargin
	}

	"sha256" in {
		test(
			"""
				|{{ 'This is a test' | sha256 }}
			""".stripMargin, false
		).trim shouldBe
			"""
				|c7be1ed902fb8dd4d48997c6452f5d7e509fbcdbe2808b16bcf4edce4c07d14e
			""".trim.stripMargin
	}

}