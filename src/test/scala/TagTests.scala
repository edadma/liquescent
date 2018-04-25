//a
package xyz.hyperreal.liquescent

import org.scalatest._
import org.scalatest.prop.PropertyChecks


class TagTests extends FreeSpec with PropertyChecks with Matchers with Testing {

	"comment" in {
		test(
			"""
				|Anything you put between {% comment %} and {% endcomment %} tags
        |is turned into a comment.
			""".stripMargin, true
		) shouldBe "Anything you put between tags is turned into a comment."
	}

  "raw" in {
    test(
      """
        |{% raw %}
        |  In Handlebars, {{ this }} will be HTML-escaped, but
        |  {{{ that }}} will not.
        |{% endraw %}
      """.stripMargin, true
    ) shouldBe "In Handlebars, {{ this }} will be HTML-escaped, but {{{ that }}} will not."
  }

}