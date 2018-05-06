//@
package xyz.hyperreal.liquescent

import xyz.hyperreal.hsl.HSL


object ExtraColorFilters {

  val map =
    List(

      new Filter( "color_to_rgb" ) {
        val hslRegex = """hsl\(\s*(\d+)\s*,\s*(\d+)\s*%\s*,\s*(\d+)\s*%\s*\)"""r
        val hslaRegex = """hsla\(\s*(\d+)\s*,\s*(\d+)\s*%\s*,\s*(\d+)\s*%\s*,\s*(\d+(?:\.\d*)?)\s*\)"""r
        val colorRegex = "#([0-9a-fA-F]{6})".r

        override def parameters = List( List(StringType) )

        override def apply( interp: Interpreter, settings: Map[Symbol, Any], args: List[Any], named: Map[String, Any], locals: Map[String, Any] ) =
          args match {
            case List( c: String ) =>
              c match {
                case hslRegex( h, s, l ) =>
                  val (r, g, b) = HSL( h.toDouble/360, s.toDouble/100, l.toDouble/100 ).toRGB

                  s"rgb($r, $g, $b)"
                case hslaRegex( h, s, l, a ) =>
                  val (r, g, b) = HSL( h.toDouble/360, s.toDouble/100, l.toDouble/100 ).toRGB

                  s"rgba($r, $g, $b, $a)"
                case colorRegex( h ) => h grouped 2 map (Integer.parseInt(_, 16)) mkString ("rgb(", ", ", ")")
                case _ => sys.error( s"color doesn't match known format: $c" )
              }
          }
      }

    ) map {f => (f.name, f)} toMap

}