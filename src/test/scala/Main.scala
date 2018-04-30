import xyz.hyperreal.liquescent.Testing


object Main extends Testing with App {

  val res =
    test(
      """
        |{{ 1.475 | money }}
      """.trim.stripMargin, false
    )

  println( s"|$res|" )

}