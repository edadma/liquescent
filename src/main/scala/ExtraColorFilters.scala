//@
package xyz.hyperreal.liquescent

import scala.collection.mutable

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

        override def apply( interp: Interpreter, settings: Map[Symbol, Any], globals: mutable.Map[String, Any], args: List[Any], named: Map[String, Any], locals: Map[String, Any] ) =
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

        override def apply( interp: Interpreter, settings: Map[Symbol, Any], globals: mutable.Map[String, Any], args: List[Any], named: Map[String, Any], locals: Map[String, Any] ) =
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

        override def apply( interp: Interpreter, settings: Map[Symbol, Any], globals: mutable.Map[String, Any], args: List[Any], named: Map[String, Any], locals: Map[String, Any] ) =
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

        override def apply( interp: Interpreter, settings: Map[Symbol, Any], globals: mutable.Map[String, Any], args: List[Any], named: Map[String, Any], locals: Map[String, Any] ) =
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

        override def apply( interp: Interpreter, settings: Map[Symbol, Any], globals: mutable.Map[String, Any], args: List[Any], named: Map[String, Any], locals: Map[String, Any] ) =
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

        override def apply( interp: Interpreter, settings: Map[Symbol, Any], globals: mutable.Map[String, Any], args: List[Any], named: Map[String, Any], locals: Map[String, Any] ) =
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
      },

      new Filter( "color_lighten" ) {
        override def parameters = List( List(StringType, NumberType) )

        override def apply( interp: Interpreter, settings: Map[Symbol, Any], globals: mutable.Map[String, Any], args: List[Any], named: Map[String, Any], locals: Map[String, Any] ) =
          args match {
            case List( c: String, v: Number ) =>
              c match {
                case rgbRegex( r, g, b ) =>
                  val hsl = HSL.fromRGB( r.toInt, g.toInt, b.toInt )
                  val (nr, ng, nb) = hsl.luminosity( hsl.l + v.intValue/100.0 ).toRGB

                  s"rgb($nr, $ng, $nb)"
                case rgbaRegex( r, g, b, a ) =>
                  val hsl = HSL.fromRGB( r.toInt, g.toInt, b.toInt )
                  val (nr, ng, nb) = hsl.luminosity( hsl.l + v.intValue/100.0 ).toRGB

                  s"rgba($nr, $ng, $nb, $a)"
                case hslRegex( h, s, l ) =>
                  val hsl = HSL( h.toDouble/360, s.toDouble/100, l.toDouble/100 )
                  val HSL( hue, sat, lum ) = hsl.luminosity( hsl.l + v.intValue/100.0 )

                  f"hsl(${hue*360}%.1f, ${sat*100}%.1f%%, ${lum*100}%.1f%%)"
                case hslaRegex( h, s, l, a ) =>
                  val hsl = HSL( h.toDouble/360, s.toDouble/100, l.toDouble/100 )
                  val HSL( hue, sat, lum ) = hsl.luminosity( hsl.l + v.intValue/100.0 )

                  f"hsla(${hue*360}%.1f, ${sat*100}%.1f%%, ${lum*100}%.1f%%, ${a.toDouble}%.3f)"
                case colorRegex( hex ) =>
                  val List( r: Int, g: Int, b: Int ) = hex grouped 2 map (Integer.parseInt(_, 16)) toList
                  val hsl = HSL.fromRGB( r.toInt, g.toInt, b.toInt )
                  val (nr, ng, nb) = hsl.luminosity( hsl.l + v.intValue/100.0 ).toRGB

                  f"#$nr%02x$ng%02x$nb%02x"
                 case _ => sys.error( s"color doesn't match known format: $c" )
              }
          }
      },

      new Filter( "color_darken" ) {
        override def parameters = List( List(StringType, NumberType) )

        override def apply( interp: Interpreter, settings: Map[Symbol, Any], globals: mutable.Map[String, Any], args: List[Any], named: Map[String, Any], locals: Map[String, Any] ) =
          args match {
            case List( c: String, v: Number ) =>
              c match {
                case rgbRegex( r, g, b ) =>
                  val hsl = HSL.fromRGB( r.toInt, g.toInt, b.toInt )
                  val (nr, ng, nb) = hsl.luminosity( hsl.l - v.intValue/100.0 ).toRGB

                  s"rgb($nr, $ng, $nb)"
                case rgbaRegex( r, g, b, a ) =>
                  val hsl = HSL.fromRGB( r.toInt, g.toInt, b.toInt )
                  val (nr, ng, nb) = hsl.luminosity( hsl.l - v.intValue/100.0 ).toRGB

                  s"rgba($nr, $ng, $nb, $a)"
                case hslRegex( h, s, l ) =>
                  val hsl = HSL( h.toDouble/360, s.toDouble/100, l.toDouble/100 )
                  val HSL( hue, sat, lum ) = hsl.luminosity( hsl.l - v.intValue/100.0 )

                  f"hsl(${hue*360}%.1f, ${sat*100}%.1f%%, ${lum*100}%.1f%%)"
                case hslaRegex( h, s, l, a ) =>
                  val hsl = HSL( h.toDouble/360, s.toDouble/100, l.toDouble/100 )
                  val HSL( hue, sat, lum ) = hsl.luminosity( hsl.l - v.intValue/100.0 )

                  f"hsla(${hue*360}%.1f, ${sat*100}%.1f%%, ${lum*100}%.1f%%, ${a.toDouble}%.3f)"
                case colorRegex( hex ) =>
                  val List( r: Int, g: Int, b: Int ) = hex grouped 2 map (Integer.parseInt(_, 16)) toList
                  val hsl = HSL.fromRGB( r.toInt, g.toInt, b.toInt )
                  val (nr, ng, nb) = hsl.luminosity( hsl.l - v.intValue/100.0 ).toRGB

                  f"#$nr%02x$ng%02x$nb%02x"
                 case _ => sys.error( s"color doesn't match known format: $c" )
              }
          }
      },

      new Filter( "color_saturate" ) {
        override def parameters = List( List(StringType, NumberType) )

        override def apply( interp: Interpreter, settings: Map[Symbol, Any], globals: mutable.Map[String, Any], args: List[Any], named: Map[String, Any], locals: Map[String, Any] ) =
          args match {
            case List( c: String, v: Number ) =>
              c match {
                case rgbRegex( r, g, b ) =>
                  val hsl = HSL.fromRGB( r.toInt, g.toInt, b.toInt )
                  val (nr, ng, nb) = hsl.saturation( hsl.s + v.intValue/100.0 ).toRGB

                  s"rgb($nr, $ng, $nb)"
                case rgbaRegex( r, g, b, a ) =>
                  val hsl = HSL.fromRGB( r.toInt, g.toInt, b.toInt )
                  val (nr, ng, nb) = hsl.saturation( hsl.s + v.intValue/100.0 ).toRGB

                  s"rgba($nr, $ng, $nb, $a)"
                case hslRegex( h, s, l ) =>
                  val hsl = HSL( h.toDouble/360, s.toDouble/100, l.toDouble/100 )
                  val HSL( hue, sat, lum ) = hsl.saturation( hsl.s + v.intValue/100.0 )

                  f"hsl(${hue*360}%.1f, ${sat*100}%.1f%%, ${lum*100}%.1f%%)"
                case hslaRegex( h, s, l, a ) =>
                  val hsl = HSL( h.toDouble/360, s.toDouble/100, l.toDouble/100 )
                  val HSL( hue, sat, lum ) = hsl.saturation( hsl.s + v.intValue/100.0 )

                  f"hsla(${hue*360}%.1f, ${sat*100}%.1f%%, ${lum*100}%.1f%%, ${a.toDouble}%.3f)"
                case colorRegex( hex ) =>
                  val List( r: Int, g: Int, b: Int ) = hex grouped 2 map (Integer.parseInt(_, 16)) toList
                  val hsl = HSL.fromRGB( r.toInt, g.toInt, b.toInt )
                  val (nr, ng, nb) = hsl.saturation( hsl.s + v.intValue/100.0 ).toRGB

                  f"#$nr%02x$ng%02x$nb%02x"
                 case _ => sys.error( s"color doesn't match known format: $c" )
              }
          }
      },

      new Filter( "color_desaturate" ) {
        override def parameters = List( List(StringType, NumberType) )

        override def apply( interp: Interpreter, settings: Map[Symbol, Any], globals: mutable.Map[String, Any], args: List[Any], named: Map[String, Any], locals: Map[String, Any] ) =
          args match {
            case List( c: String, v: Number ) =>
              c match {
                case rgbRegex( r, g, b ) =>
                  val hsl = HSL.fromRGB( r.toInt, g.toInt, b.toInt )
                  val (nr, ng, nb) = hsl.saturation( hsl.s - v.intValue/100.0 ).toRGB

                  s"rgb($nr, $ng, $nb)"
                case rgbaRegex( r, g, b, a ) =>
                  val hsl = HSL.fromRGB( r.toInt, g.toInt, b.toInt )
                  val (nr, ng, nb) = hsl.saturation( hsl.s - v.intValue/100.0 ).toRGB

                  s"rgba($nr, $ng, $nb, $a)"
                case hslRegex( h, s, l ) =>
                  val hsl = HSL( h.toDouble/360, s.toDouble/100, l.toDouble/100 )
                  val HSL( hue, sat, lum ) = hsl.saturation( hsl.s - v.intValue/100.0 )

                  f"hsl(${hue*360}%.1f, ${sat*100}%.1f%%, ${lum*100}%.1f%%)"
                case hslaRegex( h, s, l, a ) =>
                  val hsl = HSL( h.toDouble/360, s.toDouble/100, l.toDouble/100 )
                  val HSL( hue, sat, lum ) = hsl.saturation( hsl.s - v.intValue/100.0 )

                  f"hsla(${hue*360}%.1f, ${sat*100}%.1f%%, ${lum*100}%.1f%%, ${a.toDouble}%.3f)"
                case colorRegex( hex ) =>
                  val List( r: Int, g: Int, b: Int ) = hex grouped 2 map (Integer.parseInt(_, 16)) toList
                  val hsl = HSL.fromRGB( r.toInt, g.toInt, b.toInt )
                  val (nr, ng, nb) = hsl.saturation( hsl.s - v.intValue/100.0 ).toRGB

                  f"#$nr%02x$ng%02x$nb%02x"
                 case _ => sys.error( s"color doesn't match known format: $c" )
              }
          }
      },

      new Filter( "color_mix" ) {
        override def parameters = List( List(StringType, StringType, NumberType) )

        def color( c: String ) =
          c match {
            case rgbRegex( r, g, b ) => (r.toInt, g.toInt, b.toInt, 1.0)
            case rgbaRegex( r, g, b, a ) => (r.toInt, g.toInt, b.toInt, a.toDouble)
            case hslRegex( h, s, l ) =>
              val (r, g, b) = HSL( h.toDouble/360, s.toDouble/100, l.toDouble/100 ).toRGB

              (r, g, b, 1.0)
            case hslaRegex( h, s, l, a ) =>
              val (r, g, b) = HSL( h.toDouble/360, s.toDouble/100, l.toDouble/100 ).toRGB

              (r, g, b, a.toDouble)
            case colorRegex( hex ) =>
              val List( r: Int, g: Int, b: Int ) = hex grouped 2 map (Integer.parseInt(_, 16)) toList

              (r, g, b, 1.0)
          }

        def mix( r1: Int, g1: Int, b1: Int, a1: Double, r2: Int, g2: Int, b2: Int, a2: Double, factor: Int ) = {
          val f1 = factor/100.0
          val f2 = 1 - f1

          def mix( c1: Int, c2: Int ) = (c1*f1 + c2*f2).round.toInt

          (mix( r1, r2 ), mix( g1, g2 ), mix( b1, b2 ), f1*a1 + f2*a2)
        }

        override def apply( interp: Interpreter, settings: Map[Symbol, Any], globals: mutable.Map[String, Any], args: List[Any], named: Map[String, Any], locals: Map[String, Any] ) =
          args match {
            case List( c1: String, c2: String, v: Number ) =>
              c1 match {
                case rgbRegex( r, g, b ) =>
                  val (r2, g2, b2, a2) = color( c2 )
                  val (nr, ng, nb, _) = mix( r.toInt, g.toInt, b.toInt, 1, r2, g2, b2, a2, v.intValue )

                  s"rgb($nr, $ng, $nb)"
                case rgbaRegex( r, g, b, a ) =>
                  val (r2, g2, b2, a2) = color( c2 )
                  val (nr, ng, nb, na) = mix( r.toInt, g.toInt, b.toInt, a.toDouble, r2, g2, b2, a2, v.intValue )

                  s"rgba($nr, $ng, $nb, $na)"
                case hslRegex( h, s, l ) =>
                  val (r1, g1, b1) = HSL( h.toDouble/360, s.toDouble/100, l.toDouble/100 ).toRGB
                  val (r2, g2, b2, a2) = color( c2 )
                  val (nr, ng, nb, _) = mix( r1, g1, b1, 1, r2, g2, b2, a2, v.intValue )
                  val HSL( hue, sat, lum ) = HSL.fromRGB( nr, ng, nb )

                  f"hsl(${hue*360}%.1f, ${sat*100}%.1f%%, ${lum*100}%.1f%%)"
                case hslaRegex( h, s, l, a ) =>
                  val (r1, g1, b1) = HSL( h.toDouble/360, s.toDouble/100, l.toDouble/100 ).toRGB
                  val (r2, g2, b2, a2) = color( c2 )
                  val (nr, ng, nb, _) = mix( r1, g1, b1, a.toDouble, r2, g2, b2, a2, v.intValue )
                  val HSL( hue, sat, lum ) = HSL.fromRGB( nr, ng, nb )

                  f"hsla(${hue*360}%.1f, ${sat*100}%.1f%%, ${lum*100}%.1f%%, ${a.toDouble}%.3f)"
                case colorRegex( hex ) =>
                  val List( r1: Int, g1: Int, b1: Int ) = hex grouped 2 map (Integer.parseInt(_, 16)) toList
                  val (r2, g2, b2, a2) = color( c2 )
                  val (nr, ng, nb, _) = mix( r1, g1, b1, 1, r2, g2, b2, a2, v.intValue )

                  f"#$nr%02x$ng%02x$nb%02x"
                 case _ => sys.error( s"color doesn't match known format: $c1" )
              }
          }
      }

    ) map {f => (f.name, f)} toMap

}