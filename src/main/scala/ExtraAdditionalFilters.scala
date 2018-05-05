//@
package xyz.hyperreal.liquescent


object ExtraAdditionalFilters {

  val map =
    List(

      new Filter( "asset_url" ) {
        override def parameters = List( List(StringType) )

        override def apply( interp: Interpreter, settings: Map[Symbol, Any], args: List[Any], named: Map[String, Any], locals: Map[String, Any] ) =
          args match {
            case List( s: String ) =>
          }
      }
    )

}