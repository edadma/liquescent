//@
package xyz.hyperreal.liquescent


object ExtraMoneyFilters {

  val map =
    List(

      new Filter( "money" ) {
        override def parameters = List( List(NumberType) )

        override def apply( settings: Map[String, Any], args: List[Any] ) =
          args match {
            case List( n: Int ) => money( BigDecimal(n) )
            case List( n: BigInt ) => money( BigDecimal(n) )
            case List( n: BigDecimal ) => money( n )
          }

        def money( m: BigDecimal ) = ""
      }

    ) map {f => (f.name, f)} toMap

}