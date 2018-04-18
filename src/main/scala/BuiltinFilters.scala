//@
package xyz.hyperreal.fluidic

import java.time.LocalDateTime

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
        val ISO = """(\d\d\d\d-\d\d-\d\dT\d\d:\d\d:\d\d(?:.\d+)?)"""r

        override def parameters = List( List(StringType, StringType), List(DateTimeType, StringType) )

        override val invoke = {
          case List( "now"|"today", f: String ) => Strftime.format( f )
          case List( ISO(t), f: String ) => Strftime.format( f, LocalDateTime.parse(t) )
        }
      }

    ) map {f => (f.name, f)} toMap

}