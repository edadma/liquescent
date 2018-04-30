//@
package xyz.hyperreal.liquescent


object ExtraMoneyFilters {

  val amountRegex = """\{\{amount}}"""r

  def money( amount: BigDecimal, scale: Int, format: String ) = {
    val a = s"%.${scale}f".format( amount )

    amountRegex.replaceAllIn( format, a )
  }

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
          val (scale: Int, format: String) = settings('money_without_trailing_zeros)

          def output( a: BigDecimal ) =
            if (a.rounded == a)
              money( a, 0, format )
            else
              money( a, scale, format )

          args match {
            case List( n: Int ) => output( BigDecimal(n)/BigDecimal(10).pow(scale) )
            case List( n: BigInt ) => output( BigDecimal(n)/BigDecimal(10).pow(scale) )
            case List( n: BigDecimal ) => output( round(n, scale, settings) )
          }
        }
      }

    ) map {f => (f.name, f)} toMap

}