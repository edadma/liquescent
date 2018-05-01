import xyz.hyperreal.liquescent.Testing


object Main extends Testing with App {

  val res =
    test(
      """
        |{{ '#7ab55c' | color_to_rgb }}
      """.trim.stripMargin, false
    )

  println( s"|$res|" )

}