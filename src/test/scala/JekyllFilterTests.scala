//@
package xyz.hyperreal.liquescent

import org.scalatest._
import prop.PropertyChecks


class JekyllFilterTests extends FreeSpec with PropertyChecks with Matchers with Testing {

  "where" in {
		test( "{{ array | where: 'a', 'b' }}", true, "array" -> List( Map("a" -> "b", "x" -> 123), Map("u" -> 789), Map("a" -> "b", "y" -> 456) ) ) shouldBe """[{"a"=>"b","x"=>123},{"a"=>"b","y"=>456}]"""
	}

  "group_by" in {
		test( "{{ array | group_by: 'a' }}", true, "array" -> List( Map("a" -> "1", "n" -> "one"), Map("a" -> "1", "n" -> "un"), Map("a" -> "2", "n" -> "two"), Map("a" -> "2", "n" -> "deux"), Map("b" -> "1", "n" -> "one") ) ) shouldBe """[{"name"=>"2","items"=>[{"a"=>"2","n"=>"two"},{"a"=>"2","n"=>"deux"}]},{"name"=>"1","items"=>[{"a"=>"1","n"=>"one"},{"a"=>"1","n"=>"un"}]},{"name"=>null,"items"=>[{"b"=>"1","n"=>"one"}]}]"""
	}

}