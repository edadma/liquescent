//@
package xyz.hyperreal.liquescent

import java.io.{ByteArrayOutputStream, PrintStream}


trait Testing {

  val settings =
    Map(
      'docroot -> "htdocs",
      'html_without_currency -> (2, "${{amount}}"),
      'html_with_currency -> (2, "${{amount}} CAD"),
      'roundingMode -> BigDecimal.RoundingMode.HALF_EVEN,
      'locale -> "en"
    )

	def test( input: String, collapse: Boolean, assigns: (String, Any)* ) = {
		val bytes = new ByteArrayOutputStream

		new Interpreter(
      StandardFilters.map ++
        ExtraStringFilters.map ++
        ExtraHTMLFilters.map ++
        ExtraMoneyFilters.map ++
        ExtraColorFilters.map ++
        ExtraUrlFilters.map ++
        ExtraAdditionalFilters.map ++
        ExtraFilters.map,
      Map(), settings, assigns toMap, null ).
        render( LiquescentParser.parse(io.Source.fromString(input)), Map(), new PrintStream(bytes), false )

		if (collapse)
			bytes.toString.trim.replaceAll( """\s+""", " " )
		else
			bytes.toString
	}

}
