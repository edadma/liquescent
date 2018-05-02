import xyz.hyperreal.liquescent.Testing


object Main extends Testing with App {

  val res =
    test(
      """
        |{{ 1.2 | floor }}
        |{{ -1.2 | floor }}
        |{{ 2.0 | floor }}
        |{{ 183.357 | floor }}
        |{{ "3.5" | floor }}
      """.trim.stripMargin, false
    )

  println( s"|$res|" )
  println( '|' +
      """
        |1
        |-2
        |2
        |183
        |3
      """.trim.stripMargin + '|'
  )

}