//@
package xyz.hyperreal.liquescent


object ExtraFilters {

  val map =
    List(

      new Filter( "t" ) {
        override def parameters = List( List(StringType) )

        override def apply( settings: Map[Symbol, Any], args: List[Any], named: Map[String, Any] ) =
          args match {
            case List( s: String ) =>
          }
      }

    ) map {f => (f.name, f)} toMap

}