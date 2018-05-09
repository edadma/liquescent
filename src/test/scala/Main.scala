import java.time.OffsetDateTime

import xyz.hyperreal.liquescent.Testing


object Main extends Testing with App {

  val res =
    test(
      """
        |{{ time | time_tag }}
      """.trim.stripMargin, false, "time" -> OffsetDateTime.parse("2018-05-09T13:09:49.721-04:00")
    )

  println( s"|$res|" )

}