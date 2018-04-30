//@
package xyz.hyperreal.liquescent

import java.io.{ByteArrayOutputStream, PrintStream}


trait Testing {

  val settings =
    Map(
      'money_with_currency -> (2, "$%.2f CAD"),
      'roundingMode -> BigDecimal.RoundingMode.HALF_EVEN
    )

	def test( input: String, collapse: Boolean, assigns: (String, Any)* ) = {
		val bytes = new ByteArrayOutputStream

		new Interpreter( StandardFilters.map ++ ExtraStringFilters.map ++ ExtraHTMLFilters.map ++ ExtraMoneyFilters.map, Map(), settings, assigns toMap, null ).perform( LiquescentParser.parse(io.Source.fromString(input)), new PrintStream(bytes) )

		if (collapse)
			bytes.toString.trim.replaceAll( """\s+""", " " )
		else
			bytes.toString
	}

}
