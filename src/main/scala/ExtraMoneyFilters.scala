//@
package xyz.hyperreal.liquescent


object ExtraMoneyFilters {

  val map =
    List(

      new Filter( "money_with_currency" ) {
        override def parameters = List( List(NumberType) )

        override def apply( settings: Map[Symbol, Any], args: List[Any] ) = {
          val (scale: Int, format: String) = settings('money_with_currency)

          def money( m: BigDecimal ) = format.format( m )

          args match {
            case List( n: Int ) => money( BigDecimal(n)/BigDecimal(10).pow(scale) )
            case List( n: BigInt ) => money( BigDecimal(n)/BigDecimal(10).pow(scale) )
            case List( n: BigDecimal ) => money( round(n, scale, settings) )
          }
        }
      }

    ) map {f => (f.name, f)} toMap

}