//@
package xyz.hyperreal.fluidic

import java.io.{ByteArrayOutputStream, PrintStream}


trait Testing {

	def test( input: String ) = {
		val bytes = new ByteArrayOutputStream
		val out = new PrintStream(bytes)
		val interp = new Interpreter(StandardFilters.map, Map("filename" -> "/index.html", "product_price" -> 1.49, "list" -> List(Map("a" -> "asdf"), 3, 2, Map("b" -> "oops"), 4, Map("a" -> "qwer"))), out )

		FluidicParser.parse(input) foreach (op => interp.perform(op))
		bytes.toString
	}

}