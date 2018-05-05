import xyz.hyperreal.liquescent.Testing


object Main extends Testing with App {

  val res =
    test(
      """
        |{{ 'theme.css' | asset_url | stylesheet_tag }}
      """.trim.stripMargin, false
    )

  println( s"|$res|" )

}