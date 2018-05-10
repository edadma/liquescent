//@
package xyz.hyperreal.liquescent


object JekyllFilters {

  val map =
    List(

      new Filter( "where" ) {
        override def parameters = List( List(ArrayType, StringType, AnyType) )

        override def apply( interp: Interpreter, settings: Map[Symbol, Any], args: List[Any], named: Map[String, Any], locals: Map[String, Any] ) =
          args match {
            case List( array: List[Map[_, _]], prop: String, v: Any ) =>
              array.filter {
                m =>
                  m.asInstanceOf[Map[String, Any]] get prop match {
                    case None => false
                    case Some( a ) => a == v
                  }
              }
          }
      }

    ) map {f => (f.name, f)} toMap

}