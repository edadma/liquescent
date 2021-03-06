//@
package xyz.hyperreal.liquescent

import java.io.{ByteArrayOutputStream, PrintStream}
import java.nio.charset.Charset


trait Testing {

  val settings =
    Map(
      Symbol("docroot") -> "htdocs",
      Symbol("html_without_currency") -> (2, "${{amount}}"),
      Symbol("html_with_currency") -> (2, "${{amount}} CAD"),
      Symbol("roundingMode") -> BigDecimal.RoundingMode.HALF_EVEN,
      Symbol("locale") -> "en"
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
        ExtraFilters.map ++
        JekyllFilters.map,
      Map(), settings, assigns toMap, null, Charset.forName("UTF-8") ).
        render( LiquescentParser.parse(io.Source.fromString(input)), Map(), new PrintStream(bytes), false )

		if (collapse)
			bytes.toString.trim.replaceAll( """\s+""", " " )
		else
			bytes.toString
	}

}
