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

  "color_extract" in {
		test( "{{ '#7ab55c' | color_extract: 'red' }}", true ) shouldBe "122"
		test( "{{ '#7ab55c' | color_extract: 'saturation' }}", true ) shouldBe "37.6"
		test( "{{ '#7ab55c' | color_extract: 'hue' }}", true ) shouldBe "99.8"
	}

  "color_brightness" in {
		test( "{{ '#7ab55c' | color_brightness }}", true ) shouldBe "153"
	}

  "color_modify" in {
		test( "{{ '#7ab55c' | color_modify: 'red', 255 }}", true ) shouldBe "#ffb55c"
		test( "{{ '#7ab55c' | color_modify: 'alpha', 0.85 }}", true ) shouldBe "rgba(122, 181, 92, 0.85)"
	}

  "color_lighten" in {
		test( "{{ '#7ab55c' | color_lighten: 30 }}", true ) shouldBe "#d0e5c5"
		test( "{{ 'hsl(99.8, 37.6%, 53.5%)' | color_lighten: 30 }}", true ) shouldBe "hsl(99.8, 37.6%, 83.5%)"
	}

  "color_darken" in {
		test( "{{ '#7ab55c' | color_darken: 30 }}", true ) shouldBe "#355325"
		test( "{{ 'hsl(99.8, 37.6%, 53.5%)' | color_darken: 30 }}", true ) shouldBe "hsl(99.8, 37.6%, 23.5%)"
	}

  "color_saturate" in {
		test( "{{ '#7ab55c' | color_saturate: 30 }}", true ) shouldBe "#6ed938"
		test( "{{ 'hsl(99.8, 37.6%, 53.5%)' | color_saturate: 30 }}", true ) shouldBe "hsl(99.8, 67.6%, 53.5%)"
	}

  "color_desaturate" in {
		test( "{{ '#7ab55c' | color_desaturate: 30 }}", true ) shouldBe "#869180"
		test( "{{ 'hsl(99.8, 37.6%, 53.5%)' | color_desaturate: 30 }}", true ) shouldBe "hsl(99.8, 7.6%, 53.5%)"
	}

  "color_mix" in {
		test( "{{ '#7ab55c' | color_mix: '#ffc0cb', 50 }}", true ) shouldBe "#bdbb94"
		test( "{{ 'hsl(99.8, 37.6%, 53.5%)' | color_mix: '#808080', 25 }}", true ) shouldBe "hsl(98.2, 8.8%, 51.0%)"
//		test( "{{ 'rgba(122, 181, 92, 0.75)' | color_mix: '#ffc0cb', 50 }}", true ) shouldBe "rgba(189, 187, 148, 0.875)"
	}

}