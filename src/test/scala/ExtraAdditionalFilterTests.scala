//@
package xyz.hyperreal.liquescent

import java.time.OffsetDateTime

import org.scalatest._
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks


class ExtraAdditionalFilterTests extends FreeSpec with ScalaCheckPropertyChecks with Matchers with Testing {

  "time_tag" in {
		test( "{{ time | time_tag }}", true,
      "time" -> OffsetDateTime.parse("2016-02-24T09:47:51-05:00") ) shouldBe
      """<time datetime="2016-02-24T14:47:51Z">Wed., 24 Feb. 2016 09:47:51 -0500</time>"""
		test( "{{ time | time_tag: '%b %d, %Y' }}", true,
      "time" -> OffsetDateTime.parse("2016-02-24T09:47:51-05:00") ) shouldBe
      """<time datetime="2016-02-24T14:47:51Z">Feb. 24, 2016</time>"""
	}

}