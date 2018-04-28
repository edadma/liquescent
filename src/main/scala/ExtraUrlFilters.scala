//@
package xyz.hyperreal.liquescent


object ExtraUrlFilters {

  val map =
    List(

      new Filter( "asset_url" ) {
        override def parameters = List( List(StringType) )

        override val invoke = {
          case List( s: String ) =>
        }
      }
    )

}