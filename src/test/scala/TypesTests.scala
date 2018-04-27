//@
package xyz.hyperreal.liquescent

import org.scalatest._
import prop.PropertyChecks


class TypesTests extends FreeSpec with PropertyChecks with Matchers with Testing {

	"nil" in {
		test( "The current user is {{ user.name }}", false ) shouldBe "The current user is "
	}

}