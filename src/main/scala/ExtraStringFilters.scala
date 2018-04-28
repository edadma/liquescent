//@
package xyz.hyperreal.liquescent

import java.security.MessageDigest


object ExtraStringFilters {

  val nonWordRegex = """[^\w]+"""r
  val camelRegex = """-\w"""r

  def hash( s: String, dig: MessageDigest ) = dig.digest(io.Codec.toUTF8(s)) map (b => "%02x".format(b&0xFF)) mkString

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
        val md5 = MessageDigest.getInstance( "MD5" )

        override def parameters = List( List(StringType) )

        override val invoke = {
          case List( s: String ) => hash( s, md5 ) toUpperCase
        }
      },

      new Filter( "sha1" ) {
        val sha1 = MessageDigest.getInstance( "SHA-1" )

        override def parameters = List( List(StringType) )

        override val invoke = {
          case List( s: String ) => hash( s, sha1 )
        }
      },

      new Filter( "sha256" ) {
        val sha256 = MessageDigest.getInstance( "SHA-256" )

        override def parameters = List( List(StringType) )

        override val invoke = {
          case List( s: String ) => hash( s, sha256 )
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