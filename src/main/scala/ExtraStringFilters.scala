//@
package xyz.hyperreal.liquescent

import java.security.MessageDigest
import javax.crypto.spec.SecretKeySpec
import javax.crypto.Mac
import java.net.URLEncoder


object ExtraStringFilters {

  val nonWordRegex = """[^\w]+"""r
  val camelRegex = """-\w"""r

  def hex( array: Array[Byte] ) = array map (b => "%02x".format(b&0xFF)) mkString

  def hash( s: String, dig: MessageDigest ) = hex( dig.digest(io.Codec.toUTF8(s)) )

  def hmac( s: String, key: String, alg: String ) = {
    val signingKey = new SecretKeySpec( io.Codec.toUTF8(key), alg )
    val mac = Mac.getInstance( alg )

    mac init signingKey
    hex( mac doFinal io.Codec.toUTF8(s) )
  }

  val map =
    List(

      new Filter( "camelcase" ) {
        override def parameters = List( List(StringType) )

        override def apply( settings: Map[Symbol, Any], args: List[Any] ) =
          args match {
            case List( s: String ) =>
              camelRegex.replaceAllIn( s.head.toUpper + s.tail.toLowerCase, m => m.matched(1).toUpper.toString )
          }
      },

      new Filter( "handle" ) {
        override def parameters = List( List(StringType) )

        override def apply( settings: Map[Symbol, Any], args: List[Any] ) =
          args match {
            case List( s: String ) =>
              val s1 = nonWordRegex.replaceAllIn( s, _ => "-" )
              val s2 = if (s1.startsWith( "-" )) s1.substring( 1 ) else s1
              val s3 = if (s2.endsWith( "-" )) s2.substring( 0, s2.length - 1 ) else s2

              s3.toLowerCase
          }
      },

      new Filter( "handleize" ) {
        override def parameters = List( List(StringType) )

        override def apply( settings: Map[Symbol, Any], args: List[Any] ) =
          args match {
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

        override def apply( settings: Map[Symbol, Any], args: List[Any] ) =
          args match {
            case List( s: String ) => hash( s, md5 ) toUpperCase
          }
      },

      new Filter( "sha1" ) {
        val sha1 = MessageDigest.getInstance( "SHA-1" )

        override def parameters = List( List(StringType) )

        override def apply( settings: Map[Symbol, Any], args: List[Any] ) =
          args match {
            case List( s: String ) => hash( s, sha1 )
          }
      },

      new Filter( "sha256" ) {
        val sha256 = MessageDigest.getInstance( "SHA-256" )

        override def parameters = List( List(StringType) )

        override def apply( settings: Map[Symbol, Any], args: List[Any] ) =
          args match {
            case List( s: String ) => hash( s, sha256 )
          }
      },

      new Filter( "hmac_sha1" ) {
        override def parameters = List( List(StringType, StringType) )

        override def apply( settings: Map[Symbol, Any], args: List[Any] ) =
          args match {
            case List( s: String, key: String ) => hmac( s, key, "HmacSHA1" )
          }
      },

      new Filter( "hmac_sha256" ) {
        override def parameters = List( List(StringType, StringType) )

        override def apply( settings: Map[Symbol, Any], args: List[Any] ) =
          args match {
            case List( s: String, key: String ) => hmac( s, key, "HmacSHA256" )
          }
      },

      new Filter( "pluralize" ) {
        override def parameters = List( List(NumberType, StringType, StringType) )

        override def apply( settings: Map[Symbol, Any], args: List[Any] ) =
          args match {
            case List( 1, singular: String, _ ) => singular
            case List( _, _, plural: String ) => plural
          }
      },

      new Filter( "upcase" ) {
        override def parameters = List( List(StringType) )

        override def apply( settings: Map[Symbol, Any], args: List[Any] ) =
          args match {
            case List( s: String ) => s toUpperCase
          }
      },

      new Filter( "url_encode" ) {
        override def parameters = List( List(StringType) )

        override def apply( settings: Map[Symbol, Any], args: List[Any] ) =
          args match {
            case List( s: String ) => URLEncoder.encode( s, "UTF-8" )
          }
      }

    ) map {f => (f.name, f)} toMap
}

/*

      new Filter( "" ) {
        override def parameters = List( List(StringType) )

        override def apply( settings: Map[Symbol, Any], args: List[Any] ) =
          args match {
          case List( a: String ) =>
        }
      }

 */