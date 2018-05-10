import java.time.OffsetDateTime

import xyz.hyperreal.liquescent.Testing


object Main extends Testing with App {

  val res =
    test(
      """
        |{{ a | json }}
      """.trim.stripMargin, false, "a" -> Map("a" -> "b", "x" -> List[Number](123, 2.5))
    )

  println( s"|$res|" )

}