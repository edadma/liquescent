import java.time.OffsetDateTime

import xyz.hyperreal.liquescent.Testing


object Main extends Testing with App {

  val res =
    test(
      """
        |{{ array | where: "a", "b" }}
      """.trim.stripMargin, false, "array" -> List( Map("a" -> "b", "x" -> 123), Map("u" -> 789), Map("a" -> "b", "y" -> 456) )
    )

  println( s"|$res|" )

}