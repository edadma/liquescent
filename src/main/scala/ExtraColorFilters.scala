//@
package xyz.hyperreal.liquescent

import xyz.hyperreal.hsl.HSL


object ExtraColorFilters {

  val hslRegex = """hsl\(\s*(\d+)\s*,\s*(\d+)\s*%\s*,\s*(\d+)\s*%\s*\)"""r
  val hslaRegex = """hsla\(\s*(\d+)\s*,\s*(\d+)\s*%\s*,\s*(\d+)\s*%\s*,\s*(\d+(?:\.\d*)?)\s*\)"""r
  val rgbRegex = """rgb\(\s*(\d+)\s*,\s*(\d+)\s*,\s*(\d+)\s*\)"""r
  val rgbaRegex = """rgba\(\s*(\d+)\s*,\s*(\d+)\s*,\s*(\d+)\s*,\s*(\d+(?:\.\d*)?)\s*\)"""r
  val colorRegex = "#([0-9a-fA-F]{6})".r
  val map =
    List(

      new Filter( "color_to_rgb" ) {
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
      },

      new Filter( "color_to_hsl" ) {
        override def parameters = List( List(StringType) )

        override def apply( interp: Interpreter, settings: Map[Symbol, Any], args: List[Any], named: Map[String, Any], locals: Map[String, Any] ) =
          args match {
            case List( c: String ) =>
              c match {
                case rgbRegex( r, g, b ) =>
                  val HSL( h, s, l ) = HSL.fromRGB( r.toInt, g.toInt, b.toInt )

                  s"hsl($h, $s, $l)"
                case rgbaRegex( r, g, b, a ) =>
                  val HSL( h, s, l ) = HSL.fromRGB( r.toInt, g.toInt, b.toInt )

                  s"hsla($h, $s, $l, $a)"
                case colorRegex( hex ) =>
                  val List( r: Int, g: Int, b: Int ) = hex grouped 2 map (Integer.parseInt(_, 16)) toList
                  val HSL( h, s, l ) = HSL.fromRGB( r, g, b )

                  s"hsl($h, $s, $l)"
                case _ => sys.error( s"color doesn't match known format: $c" )
              }
          }
      }

    ) map {f => (f.name, f)} toMap

}