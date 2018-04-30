//@
package xyz.hyperreal.liquescent


object ExtraMoneyFilters {

  val map =
    List(

      new Filter( "money" ) {
        override def parameters = List( List(NumberType) )

        override def apply( settings: Map[String, Any], args: List[Any] ) = {
          val (places: Int, format: String) = settings("currency-money")

          def money( m: BigDecimal ) = format.format( m )

          args match {
            case List( n: Int ) => money( BigDecimal(n)/BigDecimal(10).pow(places) )
            case List( n: BigInt ) => money( BigDecimal(n)/BigDecimal(10).pow(places) )
            case List( n: BigDecimal ) => money( n )
          }
        }
      }

    ) map {f => (f.name, f)} toMap

}