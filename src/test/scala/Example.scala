import java.io.PrintStream

import scala.collection.mutable

import xyz.hyperreal.liquescent._


object Example extends App {

  val input =
    """
      |{% assign variable = "stupider" %}
      |{% ol "stupid", variable, "stupidest" %}
    """.trim.stripMargin

  val customtag =
    new Tag( "ol" ) {
      def apply(vars: mutable.Map[String, Any], out: PrintStream, args: List[Any]) =
        out.print( s"<ol>${args map (item => s"<li>$item</li>") mkString}</ol>" )
    }

  new Interpreter( StandardFilters.map, Tag(customtag), Map() ).perform( LiquescentParser.parse(io.Source.fromString(input)), Console.out )
}