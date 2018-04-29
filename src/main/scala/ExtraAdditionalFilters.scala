//@
package xyz.hyperreal.liquescent


object ExtraAdditionalFilters {

  val map =
    List(

      new Filter( "asset_url" ) {
        override def parameters = List( List(StringType) )

        override def apply( settings: Map[String, Any], args: List[Any] ) =
          args match {
            case List( s: String ) =>
          }
      }
    )

}