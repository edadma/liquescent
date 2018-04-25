import xyz.hyperreal.liquescent.Testing


object Main extends Testing with App {

  val res =
    test(
      """
        |Anything you put between {% comment %} and {% endcomment %} tags
        |is turned into a comment.
      """.stripMargin, true )

  println( s"|$res|" )

}