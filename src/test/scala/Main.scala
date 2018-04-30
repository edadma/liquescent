import xyz.hyperreal.liquescent.Testing


object Main extends Testing with App {

  val res =
    test(
      """
        |{{ 34.0 | money_without_currency }}
      """.trim.stripMargin, false
    )

  println( s"|$res|" )

}