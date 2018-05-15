//@
package xyz.hyperreal.liquescent

import org.scalatest._
import prop.PropertyChecks


class WhitespaceControlTests extends FreeSpec with PropertyChecks with Matchers with Testing {

  "no-hyphen" in {
    test(
      """
        |{% assign my_variable = "tomato" %}
        |{{ my_variable }}
      """.trim.stripMargin, false
    ) shouldBe
      """
        |
        |tomato
      """.trim.stripMargin
    test(
      """
        |{% assign username = "John G. Chalmers-Smith" %}
        |{% if username and username.size > 10 %}
        |  Wow, {{ username }}, you have a long name!
        |{% else %}
        |  Hello there!
        |{% endif %}
      """.trim.stripMargin, false
    ) shouldBe
      """
        |
        |
        |  Wow, John G. Chalmers-Smith, you have a long name!
        |
      """.trim.stripMargin
//    test(
//      """
//        |{%- assign username = "John G. Chalmers-Smith" -%}
//        |{%- if username and username.size > 10 -%}
//        |  Wow, {{ username }}, you have a long name!
//        |{%- else -%}
//        |  Hello there!
//        |{%- endif -%}
//      """.trim.stripMargin, false
//    ) shouldBe
//      """
//        |  Wow, John G. Chalmers-Smith, you have a long name!
//      """.trim.stripMargin
  }

}
