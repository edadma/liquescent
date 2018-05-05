//@
package xyz.hyperreal.liquescent


object ExtraHTMLFilters {

  val map =
    List(

      new Filter( "script_tag" ) {
        override def parameters = List( List(StringType) )

        override def apply( interp: Interpreter, settings: Map[Symbol, Any], args: List[Any], named: Map[String, Any], locals: Map[String, Any] ) =
          args match {
            case List( s: String ) => s"""<script src="$s" type="text/javascript"></script>"""
          }
      },

      new Filter( "stylesheet_tag" ) {
        override def parameters = List( List(StringType) )

        override def apply( interp: Interpreter, settings: Map[Symbol, Any], args: List[Any], named: Map[String, Any], locals: Map[String, Any] ) =
          args match {
            case List( s: String ) => s"""<link href="$s" rel="stylesheet" type="text/css" media="all" />"""
          }
      }

    ) map {f => (f.name, f)} toMap

}