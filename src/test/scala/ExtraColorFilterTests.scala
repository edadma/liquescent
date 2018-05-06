//@
package xyz.hyperreal.liquescent

import org.scalatest._
import prop.PropertyChecks


class ExtraColorFilterTests extends FreeSpec with PropertyChecks with Matchers with Testing {

  "color_to_rgb" in {
		test( "{{ '#7ab55c' | color_to_rgb }}", true ) shouldBe "rgb(122, 181, 92)"
		test( "{{ 'hsla(100, 38%, 54%, 0.5)' | color_to_rgb }}", true ) shouldBe "rgba(123, 182, 93, 0.5)"
	}

}