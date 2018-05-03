//@
package xyz.hyperreal.liquescent

import java.io.{ByteArrayOutputStream, PrintStream}


trait Testing {

  val settings =
    Map(
      'docroot -> "htdocs",
      'html_without_currency -> (2, "${{amount}}"),
      'html_with_currency -> (2, "${{amount}} CAD"),
      'roundingMode -> BigDecimal.RoundingMode.HALF_EVEN
    )

	def test( input: String, collapse: Boolean, assigns: (String, Any)* ) = {
		val bytes = new ByteArrayOutputStream

		new Interpreter(
      StandardFilters.map ++
        ExtraStringFilters.map ++
        ExtraHTMLFilters.map ++
        ExtraMoneyFilters.map ++
        ExtraColorFilters.map,
      Map(), settings, assigns toMap, null ).
        render( LiquescentParser.parse(io.Source.fromString(input)), new PrintStream(bytes), true )

		if (collapse)
			bytes.toString.trim.replaceAll( """\s+""", " " )
		else
			bytes.toString
	}

}
