//@
package xyz.hyperreal.liquescent

import org.scalatest._
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks


class TypesTests extends FreeSpec with ScalaCheckPropertyChecks with Matchers with Testing {

	"nil" in {
		test( "The current user is {{ user.name }}", false ) shouldBe "The current user is "
	}

  "bracket" in {
    test(
      """
        |{{ site.users[0] }}
        |{{ site.users[1] }}
        |{{ site.users[3] }}
      """.stripMargin, true, "site" -> Map("users" -> List("Tobi", "Laura", "Tetsuro", "Adam"))
    ) shouldBe "Tobi Laura Adam"
    test(
      """
        |{{ site['users'] | join: ", " }}
      """.stripMargin, true, "site" -> Map("users" -> List("Tobi", "Laura", "Tetsuro", "Adam"))
    ) shouldBe "Tobi, Laura, Tetsuro, Adam"
  }

}