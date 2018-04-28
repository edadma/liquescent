//@
package xyz.hyperreal.liquescent

import org.scalatest._
import prop.PropertyChecks


class ExtraHTMLFilterTests extends FreeSpec with PropertyChecks with Matchers with Testing {

  "script_tag" in {
		test( """{{ "https://code.jquery.com/jquery-3.3.1.slim.min.js" | script_tag }}""", true ) shouldBe
      """<script src="https://code.jquery.com/jquery-3.3.1.slim.min.js" type="text/javascript"></script>"""
	}

  "stylesheet_tag" in {
		test( """{{ "https://fonts.googleapis.com/css?family=Playfair+Display:700,900" | stylesheet_tag }}""", true ) shouldBe
      """<link href="https://fonts.googleapis.com/css?family=Playfair+Display:700,900" rel="stylesheet" type="text/css" media="all" />"""
	}

}