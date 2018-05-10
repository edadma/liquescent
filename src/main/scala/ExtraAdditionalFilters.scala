//@
package xyz.hyperreal.liquescent

import java.time.{OffsetDateTime, ZoneOffset}

import scala.collection.mutable

import xyz.hyperreal.strftime.Strftime
import xyz.hyperreal.json.DefaultJSONWriter


object ExtraAdditionalFilters {

  val map =
    List(

      new Filter( "time_tag" ) {
        override def parameters = List( List(TimestampType), List(TimestampType, StringType) )

        def time_tag( timestamp: OffsetDateTime, format: String ) =
          s"""<time datetime="${timestamp.atZoneSameInstant( ZoneOffset.UTC )}">${Strftime.format( format, timestamp )}</time>"""

        override def apply( interp: Interpreter, settings: Map[Symbol, Any], globals: mutable.Map[String, Any], args: List[Any], named: Map[String, Any], locals: Map[String, Any] ) =
          args match {
            case List( timestamp: OffsetDateTime ) => time_tag( timestamp, "%a, %e %b %Y %T %z" )
            case List( timestamp: OffsetDateTime, format: String ) => time_tag( timestamp, format )
          }
      },

      new Filter( "json" ) {
        override def parameters = List( List(MapType) )

        override def apply( interp: Interpreter, settings: Map[Symbol, Any], globals: mutable.Map[String, Any], args: List[Any], named: Map[String, Any], locals: Map[String, Any] ) =
          args match {
            case List( map: collection.Map[_, _] ) => DefaultJSONWriter.toString( map.asInstanceOf[collection.Map[String, Any]] )
          }
      }

    ) map {f => (f.name, f)} toMap

}