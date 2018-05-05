//@
package xyz.hyperreal.liquescent


object ExtraColorFilters {

  val map =
    List(

      new Filter( "color_to_rgb" ) {
        val colorRegex = "#[0-9a-fA-F]{6}".r.pattern

        override def parameters = List( List(StringType) )

        override def apply( interp: Interpreter, settings: Map[Symbol, Any], args: List[Any], named: Map[String, Any], locals: Map[String, String] ) =
          args match {
            case List( s: String ) =>
              if (colorRegex.matcher( s ).matches)
                s drop 1 grouped 2 map (Integer.parseInt(_, 16)) mkString ("rgb(", ", ", ")")
          }
      }

    ) map {f => (f.name, f)} toMap

}