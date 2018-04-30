//@
package xyz.hyperreal.liquescent


object ExtraMoneyFilters {

  val amountRegex = """\{\{amount}}"""r

  def currency( amount: BigDecimal, scale: Int ) = s"%.${scale}f".format(amount)

  def money( amount: BigDecimal, scale: Int, format: String ) =
    amountRegex.replaceAllIn( format, currency(amount, scale) )

  val map =
    List(

      new Filter( "money_with_currency" ) {
        override def parameters = List( List(NumberType) )

        override def apply( settings: Map[Symbol, Any], args: List[Any] ) = {
          val (scale: Int, format: String) = settings('html_with_currency)

          args match {
            case List( n: Int ) => money( BigDecimal(n)/BigDecimal(10).pow(scale), scale, format )
            case List( n: BigInt ) => money( BigDecimal(n)/BigDecimal(10).pow(scale), scale, format )
            case List( n: BigDecimal ) => money( round(n, scale, settings), scale, format )
          }
        }
      },

      new Filter( "money" ) {
        override def parameters = List( List(NumberType) )

        override def apply( settings: Map[Symbol, Any], args: List[Any] ) = {
          val (scale: Int, format: String) = settings('html_without_currency)

          args match {
            case List( n: Int ) => money( BigDecimal(n)/BigDecimal(10).pow(scale), scale, format )
            case List( n: BigInt ) => money( BigDecimal(n)/BigDecimal(10).pow(scale), scale, format )
            case List( n: BigDecimal ) => money( round(n, scale, settings), scale, format )
          }
        }
      },

      new Filter( "money_without_trailing_zeros" ) {
        override def parameters = List( List(NumberType) )

        override def apply( settings: Map[Symbol, Any], args: List[Any] ) = {
          val (scale: Int, format: String) = settings('html_with_currency)

          def output( a: BigDecimal ) =
            if (a.setScale( 0, BigDecimal.RoundingMode.HALF_EVEN ) == a)
              money( a, 0, format )
            else
              money( a, scale, format )

          args match {
            case List( n: Int ) => output( BigDecimal(n)/BigDecimal(10).pow(scale) )
            case List( n: BigInt ) => output( BigDecimal(n)/BigDecimal(10).pow(scale) )
            case List( n: BigDecimal ) => output( round(n, scale, settings) )
          }
        }
      },

      new Filter( "money_without_currency" ) {
        override def parameters = List( List(NumberType) )

        override def apply( settings: Map[Symbol, Any], args: List[Any] ) = {
          val (scale: Int, _) = settings('html_without_currency)

          args match {
            case List( n: Int ) => currency( BigDecimal(n)/BigDecimal(10).pow(scale), scale )
            case List( n: BigInt ) => currency( BigDecimal(n)/BigDecimal(10).pow(scale), scale )
            case List( n: BigDecimal ) => currency( round(n, scale, settings), scale )
          }
        }
      }

    ) map {f => (f.name, f)} toMap

}