//@
package xyz.hyperreal.liquescent

import scala.collection.mutable


abstract class Filter( val name: String, val dottable: Boolean = false ) extends ((Map[String, Any], List[Any]) => Any) {

  if (dottable)
    require( parameters.forall(_.length == 1), s"dottable filter can only take one argument: $name" )

  def parameters: List[List[Type]]

}

abstract class NumericFilter( name: String, dottable: Boolean = false ) extends Filter( name, dottable ) {

  def parameters: List[List[Type]]

  def apply( settings: Map[String, Any], args: List[Any] ) = compute( settings, args )

  def compute( settings: Map[String, Any], args: List[Any] ): Number

}