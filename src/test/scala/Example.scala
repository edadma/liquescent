//import java.io.PrintStream
//
//import scala.collection.mutable
//
//import xyz.hyperreal.liquescent._
//
//
//object Example extends App {
//
//  val input =
//    """
//      |<h2>Vaudeville Acts</h2>
//      |<ol>
//      |  {% for act in acts %}
//      |    <li>
//      |      <h3>{{ act.name }}</h3>
//      |      {% ul act.members %}
//      |    </li>
//      |  {% endfor %}
//      |</ol>
//    """.trim.stripMargin
//  val acts =
//    List(
//      Map(
//        "name" -> "Three Stooges",
//        "members" -> List( "Larry", "Moe", "Curly" )
//      ),
//      Map(
//        "name" -> "Andrews Sisters",
//        "members" -> List( "LaVerne", "Maxine", "Patty" )
//      ),
//      Map(
//        "name" -> "Abbott and Costello",
//        "members" -> List( "William (Bud) Abbott", "Lou Costello" )
//      )
//    )
//  val customtag =
//    new Tag( "ul" ) {
//      def apply( settings: Map[Symbol, Any], vars: mutable.Map[String, Any], out: PrintStream, args: List[Any], context: AnyRef ) = {
//        val list = args.head.asInstanceOf[List[String]]
//
//        out.print(s"<ul>${list map (item => s"<li>$item</li>") mkString}</ul>")
//      }
//    }
//
//  new Interpreter( StandardFilters.map, Tag(customtag), Map(), Map("acts" -> acts), null ).
//    render( LiquescentParser.parse(io.Source.fromString(input)), Map(), Console.out, false )
//}
