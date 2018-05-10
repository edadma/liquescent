//@
package xyz.hyperreal.liquescent

import org.scalatest._
import prop.PropertyChecks


class JekyllFilterTests extends FreeSpec with PropertyChecks with Matchers with Testing {

  "where" in {
		test( "{{ array | where: 'a', 'b' }}", true, "array" -> List( Map("a" -> "b", "x" -> 123), Map("u" -> 789), Map("a" -> "b", "y" -> 456) ) ) shouldBe """{"a"=>"b","x"=>123}{"a"=>"b","y"=>456}"""
	}

}