//@
package xyz.hyperreal.liquescent

import xyz.hyperreal.hsl.HSL


object ExtraColorFilters {

  val hslRegex = """hsl\(\s*(\d+(?:\.\d+)?)\s*,\s*(\d+(?:\.\d+)?)\s*%\s*,\s*(\d+(?:\.\d+)?)\s*%\s*\)"""r
  val hslaRegex = """hsla\(\s*(\d+(?:\.\d+)?)\s*,\s*(\d+(?:\.\d+)?)\s*%\s*,\s*(\d+(?:\.\d+)?)\s*%\s*,\s*(\d+(?:\.\d+)?)\s*\)"""r
  val rgbRegex = """rgb\(\s*(\d+)\s*,\s*(\d+)\s*,\s*(\d+)\s*\)"""r
  val rgbaRegex = """rgba\(\s*(\d+)\s*,\s*(\d+)\s*,\s*(\d+)\s*,\s*(\d+(?:\.\d+)?)\s*\)"""r
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
                  val (hue, sat, lum) = (h*360, s*100, l*100)

                  f"hsl($hue%.1f, $sat%.1f%%, $lum%.1f%%)"
                case rgbaRegex( r, g, b, a ) =>
                  val HSL( h, s, l ) = HSL.fromRGB( r.toInt, g.toInt, b.toInt )
                  val alpha = a.toDouble

                  f"hsla($h%.1f, $s%.1f%%, $l%.1f%%, $alpha%.3f)"
                case colorRegex( hex ) =>
                  val List( r: Int, g: Int, b: Int ) = hex grouped 2 map (Integer.parseInt(_, 16)) toList
                  val HSL( h, s, l ) = HSL.fromRGB( r, g, b )
                  val (hue, sat, lum) = (h*360, s*100, l*100)

                  f"hsl($hue%.1f, $sat%.1f%%, $lum%.1f%%)"
                case _ => sys.error( s"color doesn't match known format: $c" )
              }
          }
      },

      new Filter( "color_to_hex" ) {
        override def parameters = List( List(StringType) )

        override def apply( interp: Interpreter, settings: Map[Symbol, Any], args: List[Any], named: Map[String, Any], locals: Map[String, Any] ) =
          args match {
            case List( c: String ) =>
              c match {
                case rgbRegex( r, g, b ) => f"#${r.toInt}%02x${g.toInt}%02x${b.toInt}%02x"
                case rgbaRegex( r, g, b, _ ) => f"#${r.toInt}%02x${g.toInt}%02x${b.toInt}%02x"
                case hslRegex( h, s, l ) =>
                  val (r, g, b) = HSL( h.toDouble/360, s.toDouble/100, l.toDouble/100 ).toRGB

                  f"#${r.toInt}%02x${g.toInt}%02x${b.toInt}%02x"
                case hslaRegex( h, s, l, _ ) =>
                  val (r, g, b) = HSL( h.toDouble/360, s.toDouble/100, l.toDouble/100 ).toRGB

                  f"#${r.toInt}%02x${g.toInt}%02x${b.toInt}%02x"
                case _ => sys.error( s"color doesn't match known format: $c" )
              }
          }
      },

      new Filter( "color_extract" ) {
        def extract( h: Double, s: Double, l: Double, r: Int, g: Int, b: Int, a: Option[Double], f: String ) =
          f match {
            case "hue" => f"$h%.1f"
            case "saturation" => f"$s%.1f"
            case "luminosity" => f"$l%.1f"
            case "red" => r.toString
            case "green" => g.toString
            case "blue" => b.toString
            case "alpha" => f"${a.get}%.3f"
          }

        override def parameters = List( List(StringType, StringType) )

        override def apply( interp: Interpreter, settings: Map[Symbol, Any], args: List[Any], named: Map[String, Any], locals: Map[String, Any] ) =
          args match {
            case List( c: String, f: String ) =>
              c match {
                case rgbRegex( r, g, b ) =>
                  val red = r.toInt
                  val green = g.toInt
                  val blue = b.toInt
                  val HSL( h, s, l ) = HSL.fromRGB( red, green, blue )

                  extract( h*360, s*100, l*100, red, green, blue, None, f )
                case rgbaRegex( r, g, b, a ) =>
                  val red = r.toInt
                  val green = g.toInt
                  val blue = b.toInt
                  val HSL( h, s, l ) = HSL.fromRGB( red, green, blue )

                  extract( h*360, s*100, l*100, red, green, blue, Some(a.toDouble), f )
                case hslRegex( h, s, l ) =>
                  val hue = h.toDouble
                  val sat = s.toDouble
                  val lum = l.toDouble
                  val (r, g, b) = HSL( hue/360, sat/100, lum/100 ).toRGB

                  extract( hue, sat, lum, r, g, b, None, f )
                case hslaRegex( h, s, l, a ) =>
                  val hue = h.toDouble
                  val sat = s.toDouble
                  val lum = l.toDouble
                  val (r, g, b) = HSL( hue/360, sat/100, lum/100 ).toRGB

                  extract( hue, sat, lum, r, g, b, Some(a.toDouble), f )
                case colorRegex( hex ) =>
                  val List( r: Int, g: Int, b: Int ) = hex grouped 2 map (Integer.parseInt(_, 16)) toList
                  val HSL( h, s, l ) = HSL.fromRGB( r, g, b )

                  extract( h*360, s*100, l*100, r, g, b, None, f )
                case _ => sys.error( s"color doesn't match known format: $c" )
              }
          }
      },

      new Filter( "color_brightness" ) {

        def brightness( r: Int, g: Int, b: Int ) = (r*299 + g*587 + b*114)/1000

        override def parameters = List( List(StringType) )

        override def apply( interp: Interpreter, settings: Map[Symbol, Any], args: List[Any], named: Map[String, Any], locals: Map[String, Any] ) =
          args match {
            case List( c: String ) =>
              c match {
                case rgbRegex( r, g, b ) => brightness( r.toInt, g.toInt, b.toInt )
                case rgbaRegex( r, g, b, _ ) => brightness( r.toInt, g.toInt, b.toInt )
                case hslRegex( h, s, l ) =>
                  val hue = h.toDouble
                  val sat = s.toDouble
                  val lum = l.toDouble
                  val (r, g, b) = HSL( hue/360, sat/100, lum/100 ).toRGB

                  brightness( r, g, b )
                case hslaRegex( h, s, l, _ ) =>
                  val hue = h.toDouble
                  val sat = s.toDouble
                  val lum = l.toDouble
                  val (r, g, b) = HSL( hue/360, sat/100, lum/100 ).toRGB

                  brightness( r, g, b )
                case colorRegex( hex ) =>
                  val List( r: Int, g: Int, b: Int ) = hex grouped 2 map (Integer.parseInt(_, 16)) toList

                  brightness( r, g, b )
                case _ => sys.error( s"color doesn't match known format: $c" )
              }
          }
      },

      new Filter( "color_modify" ) {
        def modify( h: Double, s: Double, l: Double, r: Int, g: Int, b: Int, a: Option[Double], f: String, v: Number ) =
          (a, f) match {
            case (None, "hue") => f"hsl(${v.doubleValue}%.1f, $s%.1f%%, $l%.1f%%)"
            case (Some( alpha ), "hue") => f"hsla(${v.doubleValue}%.1f, $s%.1f%%, $l%.1f%%, $alpha%.3f)"
            case (None, "saturation") => f"hsl($h%.1f, ${v.doubleValue}%.1f%%, $l%.1f%%)"
            case (Some( alpha ), "saturation") => f"hsl($h%.1f, ${v.doubleValue}%.1f%%, $l%.1f%%, $alpha%.3f)"
            case (None, "luminosity") => f"hsl($h%.1f, $s%.1f%%, ${v.doubleValue}%.1f%%)"
            case (Some( alpha ), "luminosity") => f"hsl($h%.1f, $s%.1f%%, ${v.doubleValue}%.1f%%, $alpha%.3f)"
            case (None, "red") => f"#${v.intValue}%02x$g%02x$b%02x"
            case (Some( alpha ), "red") => s"rgba(${v.intValue}, $g, $b, $alpha)"
            case (None, "green") => f"#$r%02x${v.intValue}%02x$b%02x"
            case (Some( alpha ), "green") => s"rgba($r, ${v.intValue}, $b, $alpha)"
            case (None, "blue") => f"#$r%02x$g%02x${v.intValue}%02x"
            case (Some( alpha ), "blue") => s"rgba($r, $g, ${v.intValue}, $alpha)"
            case (_, "alpha") => s"rgba($r, $g, $b, ${v.doubleValue})"
          }

        override def parameters = List( List(StringType, StringType, NumberType) )

        override def apply( interp: Interpreter, settings: Map[Symbol, Any], args: List[Any], named: Map[String, Any], locals: Map[String, Any] ) =
          args match {
            case List( c: String, f: String, v: Number ) =>
              c match {
                case rgbRegex( r, g, b ) =>
                  val red = r.toInt
                  val green = g.toInt
                  val blue = b.toInt
                  val HSL( h, s, l ) = HSL.fromRGB( red, green, blue )

                  modify( h*360, s*100, l*100, red, green, blue, None, f, v )
                case rgbaRegex( r, g, b, a ) =>
                  val red = r.toInt
                  val green = g.toInt
                  val blue = b.toInt
                  val HSL( h, s, l ) = HSL.fromRGB( red, green, blue )

                  modify( h*360, s*100, l*100, red, green, blue, Some(a.toDouble), f, v )
                case hslRegex( h, s, l ) =>
                  val hue = h.toDouble
                  val sat = s.toDouble
                  val lum = l.toDouble
                  val (r, g, b) = HSL( hue/360, sat/100, lum/100 ).toRGB

                  modify( hue, sat, lum, r, g, b, None, f, v )
                case hslaRegex( h, s, l, a ) =>
                  val hue = h.toDouble
                  val sat = s.toDouble
                  val lum = l.toDouble
                  val (r, g, b) = HSL( hue/360, sat/100, lum/100 ).toRGB

                  modify( hue, sat, lum, r, g, b, Some(a.toDouble), f, v )
                case colorRegex( hex ) =>
                  val List( r: Int, g: Int, b: Int ) = hex grouped 2 map (Integer.parseInt(_, 16)) toList
                  val HSL( h, s, l ) = HSL.fromRGB( r, g, b )

                  modify( h*360, s*100, l*100, r, g, b, None, f, v )
                case _ => sys.error( s"color doesn't match known format: $c" )
              }
          }
      }

    ) map {f => (f.name, f)} toMap

}