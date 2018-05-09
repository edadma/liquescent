//@
package xyz.hyperreal.liquescent

import java.time.{OffsetDateTime, ZoneOffset}
import java.time.temporal.TemporalAccessor

import xyz.hyperreal.strftime.Strftime


object ExtraAdditionalFilters {

  val map =
    List(

      new Filter( "time_tag" ) {
        override def parameters = List( List(TimestampType), List(TimestampType, StringType) )

        def time_tag( timestamp: OffsetDateTime, format: String ) =
          s"""<time datetime="${timestamp.atZoneSameInstant( ZoneOffset.UTC )}">${Strftime.format( format, timestamp )}</time>"""

        override def apply( interp: Interpreter, settings: Map[Symbol, Any], args: List[Any], named: Map[String, Any], locals: Map[String, Any] ) =
          args match {
            case List( timestamp: OffsetDateTime ) => time_tag( timestamp, "%a, %e %b %Y %T %z" )
            case List( timestamp: OffsetDateTime, format: String ) => time_tag( timestamp, format )
          }
      }

    ) map {f => (f.name, f)} toMap

}