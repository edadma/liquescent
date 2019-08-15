//@
package xyz.hyperreal.liquescent

import org.scalatest._
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks


class ExtraStringFilterTests extends FreeSpec with ScalaCheckPropertyChecks with Matchers with Testing {

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

	"hmac_sha1" in {
		test(
			"""
				|{% assign my_secret_string = "ShopifyIsAwesome!" | hmac_sha1: "secret_key" %}
        |My encoded string is: {{ my_secret_string }}
			""".stripMargin, false
		).trim shouldBe
			"""
				|My encoded string is: 30ab3459e46e7b209b45dba8378fcbba67297304
			""".trim.stripMargin
	}

	"hmac_sha256" in {
		test(
			"""
				|{% assign my_secret_string = "This is a test." | hmac_sha256: "Secret Key" %}
        |My encoded string is: {{ my_secret_string }}
			""".stripMargin, false
		).trim shouldBe
			"""
				|My encoded string is: d28cda261fadb21e751aaab08e25de526b53c491117978bda57152e051e1bf3f
			""".trim.stripMargin
	}

	"pluralize" in {
		test(
			"""
				|{{ cart.item_count }}
        |{{ cart.item_count | pluralize: 'item', 'items' }}
			""".stripMargin, true, "cart" -> Map("item_count" -> 3)
		) shouldBe "3 items"
		test(
			"""
				|{{ cart.item_count }}
        |{{ cart.item_count | pluralize: 'item', 'items' }}
			""".stripMargin, true, "cart" -> Map("item_count" -> 0)
		) shouldBe "0 items"
		test(
			"""
				|{{ cart.item_count }}
        |{{ cart.item_count | pluralize: 'item', 'items' }}
			""".stripMargin, true, "cart" -> Map("item_count" -> 1)
		) shouldBe "1 item"
	}

	"upcase" in {
		test(
			"""
				|{{ "i want this to be uppercase" | upcase }}
			""".stripMargin, true
		) shouldBe "I WANT THIS TO BE UPPERCASE"
	}

	"url_encode" in {
		test(
			"""
				|{{ 'john@liquid.com' | url_encode }}
			""".stripMargin, true
		) shouldBe "john%40liquid.com"
		test(
			"""
				|{{ 'Tetsuro Takara' | url_encode }}
			""".stripMargin, true
		) shouldBe "Tetsuro+Takara"
	}

}