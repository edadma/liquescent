//@
package xyz.hyperreal.liquescent

import org.scalatest._
import prop.PropertyChecks


class ExtraMoneyFilterTests extends FreeSpec with PropertyChecks with Matchers with Testing {

  "money" in {
		test( "{{ 145 | money }}", true ) shouldBe "$1.45"
	}

  "money_with_currency" in {
		test( "{{ 145 | money_with_currency }}", true ) shouldBe "$1.45 CAD"
	}

  "money_without_trailing_zeros" in {
		test( "{{ 2000 | money_without_trailing_zeros }}", true ) shouldBe "$20 CAD"
		test( "{{ 145 | money_without_trailing_zeros }}", true ) shouldBe "$1.45 CAD"
	}

  "money_without_currency" in {
		test( "{{ 2000 | money_without_currency }}", true ) shouldBe "20.00"
		test( "{{ 145 | money_without_currency }}", true ) shouldBe "1.45"
		test( "{{ 20.0 | money_without_currency }}", true ) shouldBe "20.00"
		test( "{{ 1.45 | money_without_currency }}", true ) shouldBe "1.45"
	}

}