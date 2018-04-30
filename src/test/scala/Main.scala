import xyz.hyperreal.liquescent.Testing


object Main extends Testing with App {

  val res =
    test(
      """
        |{{ 3410 | money_without_currency }}
      """.trim.stripMargin, false
    )

  println( s"|$res|" )

}