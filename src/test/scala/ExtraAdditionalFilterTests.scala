//@
package xyz.hyperreal.liquescent

import java.time.OffsetDateTime

import org.scalatest._
import prop.PropertyChecks


class ExtraAdditionalFilterTests extends FreeSpec with PropertyChecks with Matchers with Testing {

  "time_tag" in {
		test( "{{ time | time_tag }}", true,
      "time" -> OffsetDateTime.parse("2018-05-09T17:09:49.721Z") ) shouldBe
      """<time datetime="2018-05-09T17:09:49.721Z">May 09, 2018</time>"""
		test( "{{ time | time_tag: '%b %d, %Y' }}", true,
      "time" -> OffsetDateTime.parse("2018-05-09T17:09:49.721Z") ) shouldBe
      """<time datetime="2018-05-09T17:09:49.721Z">May 09, 2018</time>"""
	}

}