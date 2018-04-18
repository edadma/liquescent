//@
package xyz.hyperreal.fluidic

import java.time.{LocalDate, LocalDateTime}
import java.time.format.DateTimeFormatter

import math._
import xyz.hyperreal.strftime.Strftime


object BuiltinFilters {

  val map =
    List(

      new NumericFilter( "abs" ) {
        override def parameters = List( List(NumberType) )

        override val perform = {
          case List( a: Long ) => abs( a )
          case List( a: Double ) => abs( a )
        }
      },

      new Filter( "append" ) {
        override def parameters = List( List(StringType, StringType) )

        override val invoke = {
          case List( l: String, r: String ) => l + r
        }
      },

      new Filter( "date" ) {
        val ISO_DATETIME_REGEX = """\d\d\d\d-\d\d-\d\dT\d\d:\d\d:\d\d(?:.\d+)?"""r
        val USUAL_DATETIME_REGEX = """[a-zA-Z]+ \d+, \d+"""r
        val USUAL_DATETIME_FORMAT = DateTimeFormatter.ofPattern( "MMMM dd, yyyy" )

        override def parameters = List( List(StringType, StringType), List(DateTimeType, StringType) )

        override val invoke = {
          case List( "now"|"today", f: String ) => Strftime.format( f )
          case List( t: String, f: String ) =>
            if (ISO_DATETIME_REGEX.pattern.matcher( t ).matches)
              Strftime.format( f, LocalDateTime.parse(t) )
            else if (USUAL_DATETIME_REGEX.pattern.matcher( t ).matches)
              Strftime.format( f, LocalDate.parse(t, USUAL_DATETIME_FORMAT) )
            else
              sys.error( s"unrecognized date/time format: $t" )
        }
      }

    ) map {f => (f.name, f)} toMap

}