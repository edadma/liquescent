//@
package xyz.hyperreal.liquescent

import java.io.{ByteArrayOutputStream, PrintStream}


trait Testing {

	def test( input: String, collapse: Boolean, assigns: (String, Any)* ) = {
		val bytes = new ByteArrayOutputStream
		val interp = new Interpreter(StandardFilters.map, assigns toMap )

		interp.perform( LiquescentParser.parse(input), new PrintStream(bytes) )

		if (collapse)
			bytes.toString.trim.replaceAll( """\s+""", " " )
		else
			bytes.toString
	}

}
