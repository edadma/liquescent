//@
package xyz.hyperreal.liquescent

import scala.collection.mutable


abstract class Filter( val name: String, val dottable: Boolean = false ) extends ((Map[Symbol, Any], List[Any], Map[String, Any]) => Any) {

  if (dottable)
    require( parameters.forall(_.length == 1), s"dottable filter can only take one argument: $name" )

  def parameters: List[List[Type]]

}

abstract class NumericFilter( name: String, dottable: Boolean = false ) extends Filter( name, dottable ) {

  def parameters: List[List[Type]]

  def apply( settings: Map[Symbol, Any], args: List[Any], named: Map[String, Any] ) = compute( settings, args, named )

  def compute( settings: Map[Symbol, Any], args: List[Any], named: Map[String, Any] ): Number

}