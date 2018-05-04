import xyz.hyperreal.liquescent.Testing


object Main extends Testing with App {

  val res =
    test(
      """
        |{{ 'general.404.title' | t }}
      """.trim.stripMargin, false
    )

  println( s"|$res|" )

}