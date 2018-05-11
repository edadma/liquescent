//@
package xyz.hyperreal.liquescent

import org.scalatest._
import prop.PropertyChecks


class ExtraUrlFilterTests extends FreeSpec with PropertyChecks with Matchers with Testing {

  "link_to" in {
    test( "{{ 'Asdf' | link_to: 'https://www.asdf.com', 'A link to Asdf' }}", true ) shouldBe
      """<a href="https://www.asdf.com" title="A link to Asdf">Asdf</a>"""
  }

}