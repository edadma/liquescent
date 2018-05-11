//@
package xyz.hyperreal.liquescent

import scala.collection.mutable


object ExtraUrlFilters {

  val map =
    List(

      new Filter( "asset_url" ) {
        override def parameters = List( List(StringType) )

        override def apply( interp: Interpreter, settings: Map[Symbol, Any], globals: mutable.Map[String, Any], args: List[Any], named: Map[String, Any], locals: Map[String, Any] ) =
          args match {
            case List( s: String ) =>
              "assets/" + s
          }
      },

      new Filter( "link_to" ) {
        override def parameters = List( List(StringType, StringType), List(StringType, StringType, StringType) )

        override def apply( interp: Interpreter, settings: Map[Symbol, Any], globals: mutable.Map[String, Any], args: List[Any], named: Map[String, Any], locals: Map[String, Any] ) =
          args match {
            case List( s: String, url: String ) => s"""<a href="$url">$s</a>"""
            case List( s: String, url: String, title: String ) => s"""<a href="$url" title="$title">$s</a>"""
          }
      }

    ) map {f => (f.name, f)} toMap

}