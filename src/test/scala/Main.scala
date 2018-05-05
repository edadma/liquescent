import xyz.hyperreal.liquescent.Testing


object Main extends Testing with App {

  val res =
    test(
      """
        |{{ 'general.meta.tags' | t: tags: 'wow' }}
      """.trim.stripMargin, false
    )

  println( s"|$res|" )

}