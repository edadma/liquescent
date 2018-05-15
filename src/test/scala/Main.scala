import java.time.OffsetDateTime

import xyz.hyperreal.liquescent.{LiquescentParser, Testing}


object Main extends Testing with App {

  val res =
    test( """{{ "-19.86" }}""", false, "asdf" -> "qwer" )

  println( s"|$res|" )

}


//      """
//        |{{ array | group_by: 'a' }}
//      """.trim.stripMargin, false, "array" -> List( Map("a" -> "1", "n" -> "one"), Map("a" -> "1", "n" -> "un"), Map("a" -> "2", "n" -> "two"), Map("a" -> "2", "n" -> "deux"), Map("b" -> "1", "n" -> "one") )
//    )
