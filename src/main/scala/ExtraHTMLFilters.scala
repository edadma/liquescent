//@
package xyz.hyperreal.liquescent

import scala.collection.mutable


object ExtraHTMLFilters {

  val map =
    List(

      new Filter( "img_tag" ) {
        override def parameters = List( List(StringType), List(StringType, StringType), List(StringType, StringType, StringType) )

        override def apply( interp: Interpreter, settings: Map[Symbol, Any], globals: mutable.Map[String, Any], args: List[Any], named: Map[String, Any], locals: Map[String, Any] ) =
          args match {
            case List( src: String ) => s"""<img src="$src" />"""
            case List( src: String, alt: String ) => s"""<img src="$src" alt="$alt" />"""
            case List( src: String, alt: String, cls: String ) => s"""<img src="$src" alt="$alt" class="$cls" />"""
          }
      },

      new Filter( "script_tag" ) {
        override def parameters = List( List(StringType) )

        override def apply( interp: Interpreter, settings: Map[Symbol, Any], globals: mutable.Map[String, Any], args: List[Any], named: Map[String, Any], locals: Map[String, Any] ) =
          args match {
            case List( s: String ) => s"""<script src="$s" type="text/javascript"></script>"""
          }
      },

      new Filter( "stylesheet_tag" ) {
        override def parameters = List( List(StringType) )

        override def apply( interp: Interpreter, settings: Map[Symbol, Any], globals: mutable.Map[String, Any], args: List[Any], named: Map[String, Any], locals: Map[String, Any] ) =
          args match {
            case List( s: String ) => s"""<link href="$s" rel="stylesheet" type="text/css" media="all" />"""
          }
      }

    ) map {f => (f.name, f)} toMap

}