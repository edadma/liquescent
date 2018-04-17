//@
package xyz.hyperreal.fluidic

import math._


object BuiltinFilters {

  val list =
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
      }

    )

}