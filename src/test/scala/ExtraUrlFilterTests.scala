//@
package xyz.hyperreal.liquescent

import org.scalatest._
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks


class ExtraUrlFilterTests extends FreeSpec with ScalaCheckPropertyChecks with Matchers with Testing {

  "link_to" in {
    test( "{{ 'Asdf' | link_to: 'https://www.asdf.com', 'A link to Asdf' }}", true ) shouldBe
      """<a href="https://www.asdf.com" title="A link to Asdf">Asdf</a>"""
  }

}