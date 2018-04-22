//@
package xyz.hyperreal.fluidic

import java.io.{ByteArrayOutputStream, PrintStream}


trait Testing {

	def test( input: String ) = {
		val bytes = new ByteArrayOutputStream
		val interp = new Interpreter(StandardFilters.map, Map("article" -> Map("published_at" -> "2015-07-17"), "filename" -> "/index.html", "product_price" -> 1.49, "list" -> List(Map("a" -> "asdf"), 3, 2, Map("b" -> "oops"), 4, Map("a" -> "qwer"))) )

		interp.perform( FluidicParser.parse(input), new PrintStream(bytes) )
		bytes.toString
	}

}