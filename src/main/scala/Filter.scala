//@
package xyz.hyperreal.fluidic


abstract class Filter( val name: String, dottable: Boolean = false ) {

  def parameters: List[List[Type]]

  val invoke: List[Any] => AnyRef

}

abstract class NumericFilter( val name: String, dottable: Boolean = false ) {

  def parameters: List[List[Type]]

  def invoke( args: List[Any] ) = perform( args )

  val perform: List[Any] => Number

}