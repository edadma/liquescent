//@
package xyz.hyperreal.liquescent

import java.time.ZonedDateTime


object ExtraAdditionalFilters {

  val map =
    List(

      new Filter( "time_tag" ) {
        override def parameters = List( List(TimestampType), List(TimestampType, StringType) )

        override def apply( interp: Interpreter, settings: Map[Symbol, Any], args: List[Any], named: Map[String, Any], locals: Map[String, Any] ) =
          args match {
            case List( timestamp: ZonedDateTime ) =>
            case List( timestamp: ZonedDateTime, format: String ) =>
          }
      }
    )

}