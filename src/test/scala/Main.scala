import xyz.hyperreal.liquescent.Testing


object Main extends Testing with App {

  val res =
    test(
      """
        |{%- assign username = "John G. Chalmers-Smith" -%}
        |{%- if username and username.size > 10 -%}
        |  Wow, {{ username }}, you have a long name!
        |{%- else -%}
        |  Hello there!
        |{%- endif -%}
      """.trim.stripMargin, false
    )

  println( s"|$res|" )

}