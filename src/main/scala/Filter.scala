//@
package xyz.hyperreal.fluidic


abstract class Filter( val name: String, val dottable: Boolean = false ) {

  if (dottable)
    require( parameters.forall(_.length == 1), s"dottable filter can only take one argument: $name" )

  def parameters: List[List[Type]]

  val invoke: List[Any] => AnyRef

}

abstract class NumericFilter( name: String, dottable: Boolean = false ) extends Filter( name, dottable ) {

  def parameters: List[List[Type]]

  lazy val invoke = compute

  val compute: List[Any] => Number

}