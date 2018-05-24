//@
package xyz.hyperreal.liquescent

import java.io.File

import scala.collection.mutable


object ExtraUrlFilters {

  val sizeRegex = """(\d*)x(\d*)"""r

  val map =
    List(

      new Filter( "asset_img_url" ) {
        override def parameters = List( List(StringType, StringType) )

        override def apply( interp: Interpreter, settings: Map[Symbol, Any], globals: mutable.Map[String, Any], args: List[Any], named: Map[String, Any], locals: Map[String, Any] ) =
          args match {
            case List( file: String, size: String ) =>
              size match {
                case sizeRegex( x, y ) if x.nonEmpty || y.nonEmpty =>
                  val in = s"${File.separator}assets${File.separator}$file"

                case _ => sys.error( s"expected image size parameter (<width>x<height>): $size" )
              }

          }
      },

      new Filter( "asset_url" ) {
        override def parameters = List( List(StringType) )

        override def apply( interp: Interpreter, settings: Map[Symbol, Any], globals: mutable.Map[String, Any], args: List[Any], named: Map[String, Any], locals: Map[String, Any] ) =
          args match {
            case List( s: String ) => s"/assets/$s"
          }
      },

      new Filter( "file_url" ) {
        override def parameters = List( List(StringType) )

        override def apply( interp: Interpreter, settings: Map[Symbol, Any], globals: mutable.Map[String, Any], args: List[Any], named: Map[String, Any], locals: Map[String, Any] ) =
          args match {
            case List( s: String ) => s"/files/$s"
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