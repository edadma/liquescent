//@
package xyz.hyperreal.liquescent

import org.scalatest._
import prop.PropertyChecks


class ExtraColorFilterTests extends FreeSpec with PropertyChecks with Matchers with Testing {

  "color_to_rgb" in {
		test( "{{ '#7ab55c' | color_to_rgb }}", true ) shouldBe "rgb(122, 181, 92)"
		test( "{{ 'hsla(100, 38%, 54%, 0.5)' | color_to_rgb }}", true ) shouldBe "rgba(123, 182, 93, 0.5)"
	}

  "color_to_hsl" in {
		test( "{{ '#7ab55c' | color_to_hsl }}", true ) shouldBe "hsl(99.8, 37.6%, 53.5%)"
		test( "{{ 'rgb(122, 181, 92)' | color_to_hsl }}", true ) shouldBe "hsl(99.8, 37.6%, 53.5%)"
	}

  "color_to_hex" in {
		test( "{{ 'rgba(122, 181, 92, 0.5)' | color_to_hex }}", true ) shouldBe "#7ab55c"
		test( "{{ 'rgb(122, 181, 92)' | color_to_hex }}", true ) shouldBe "#7ab55c"
	}

}