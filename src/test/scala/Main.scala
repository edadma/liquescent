import xyz.hyperreal.liquescent.Testing


object Main extends Testing with App {

  val res =
    test(
      """
        |{% assign test = 'cute' %}
        |{% include 'test' %}
      """.trim.stripMargin, false
    )

  println( s"|$res|" )

}