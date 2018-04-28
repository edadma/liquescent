//@
package xyz.hyperreal.liquescent

import java.security.MessageDigest


object ExtraStringFilters {

  val nonWordRegex = """[^\w]+"""r

  val camelRegex = """-\w"""r

  val map =
    List(

      new Filter( "camelcase" ) {
        override def parameters = List( List(StringType) )

        override val invoke = {
          case List( s: String ) =>
            camelRegex.replaceAllIn( s.head.toUpper + s.tail.toLowerCase, m => m.matched(1).toUpper.toString )
        }
      },

      new Filter( "handle" ) {
        override def parameters = List( List(StringType) )

        override val invoke = {
          case List( s: String ) =>
            val s1 = nonWordRegex.replaceAllIn( s, _ => "-" )
            val s2 = if (s1.startsWith( "-" )) s1.substring( 1 ) else s1
            val s3 = if (s2.endsWith( "-" )) s2.substring( 0, s2.length - 1 ) else s2

            s3.toLowerCase
        }
      },

      new Filter( "handleize" ) {
        override def parameters = List( List(StringType) )

        override val invoke = {
          case List( s: String ) =>
            val s1 = nonWordRegex.replaceAllIn( s, _ => "-" )
            val s2 = if (s1.startsWith( "-" )) s1.substring( 1 ) else s1
            val s3 = if (s2.endsWith( "-" )) s2.substring( 0, s2.length - 1 ) else s2

            s3.toLowerCase
        }
      },

      new Filter( "md5" ) {
        override def parameters = List( List(StringType) )

        override val invoke = {
          case List( s: String ) =>

            camelRegex.replaceAllIn( s.head.toUpper + s.tail.toLowerCase, m => m.matched(1).toUpper.toString )
        }
      }

    ) map {f => (f.name, f)} toMap
}

/*

      new Filter( "" ) {
        override def parameters = List( List(StringType) )

        override val invoke = {
          case List( a: String ) =>
        }
      }

 */