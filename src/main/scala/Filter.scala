//@
package xyz.hyperreal.liquescent

import scala.collection.mutable


abstract class Filter( val name: String, val dottable: Boolean = false ) extends ((Interpreter, Map[Symbol, Any], List[Any], Map[String, Any], Map[String, Any]) => Any) {

  if (dottable)
    require( parameters.forall(_.length == 1), s"dottable filter can only take one argument: $name" )

  def parameters: List[List[Type]]

}

abstract class NumericFilter( name: String, dottable: Boolean = false ) extends Filter( name, dottable ) {

  def parameters: List[List[Type]]

  def apply( interp: Interpreter, settings: Map[Symbol, Any], args: List[Any], named: Map[String, Any], locals: Map[String, Any] ) = compute( interp, settings, args, named, locals )

  def compute( interp: Interpreter, settings: Map[Symbol, Any], args: List[Any], named: Map[String, Any], locals: Map[String, Any] ): Number

}