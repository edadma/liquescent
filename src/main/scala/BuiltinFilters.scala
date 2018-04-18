//@
package xyz.hyperreal.fluidic

import java.time.{LocalDate, LocalDateTime}
import java.time.format.DateTimeFormatter

import math._
import xyz.hyperreal.strftime.Strftime

import scala.collection.mutable
import scala.collection.mutable.ListBuffer


object BuiltinFilters {

  val map =
    List(

      new NumericFilter( "abs" ) {
        override def parameters = List( List(NumberType) )

        override val compute = {
          case List( a: Long ) => abs( a )
          case List( a: Double ) => abs( a )
        }
      },

      new Filter( "append" ) {
        override def parameters = List( List(StringType, StringType) )

        override val invoke = {
          case List( l: String, r: String ) => l + r
        }
      },

      new Filter( "capitalize" ) {
        override def parameters = List( List(StringType) )

        override val invoke = {
          case List( s: String ) => s.head.toUpper + s.tail.toLowerCase
        }
      },

      new Filter( "compact", true ) {
        override def parameters = List( List(ArrayType) )

        override val invoke = {
          case List( l: List[_] ) => l filterNot (_ == nil)
        }
      },

      new Filter( "concat" ) {
        override def parameters = List( List(ArrayType, ArrayType) )

        override val invoke = {
          case List( l: List[_], r: List[_] ) => l ++ r
        }
      },

      new Filter( "date" ) {
        val ISO_DATETIME_REGEX = """\d\d\d\d-\d\d-\d\dT\d\d:\d\d:\d\d(?:.\d+)?""" r
        val USUAL_DATETIME_REGEX = """[a-zA-Z]+ \d+, \d+""" r
        val USUAL_DATETIME_FORMAT = DateTimeFormatter.ofPattern("MMMM dd, yyyy")

        override def parameters = List(List(StringType, StringType), List(DateTimeType, StringType))

        override val invoke = {
          case List("now" | "today", f: String) => Strftime.format(f)
          case List(t: String, f: String) =>
            if (ISO_DATETIME_REGEX.pattern.matcher(t).matches)
              Strftime.format(f, LocalDateTime.parse(t))
            else if (USUAL_DATETIME_REGEX.pattern.matcher(t).matches)
              Strftime.format(f, LocalDate.parse(t, USUAL_DATETIME_FORMAT))
            else
              sys.error(s"unrecognized date/time format: $t")
        }
      },

      new Filter( "downcase" ) {
        override def parameters = List( List(StringType) )

        override val invoke = {
          case List( s: String ) => s.toLowerCase
        }
      },

      new Filter( "first", true ) {
        override def parameters = List( List(ArrayType) )

        override val invoke = {
          case List( l: List[_] ) => l.asInstanceOf[List[AnyRef]].head
        }
      },

      new Filter( "join" ) {
        override def parameters = List( List(ArrayType, StringType) )

        override val invoke = {
          case List( l: List[_], s: String ) => l map display mkString s
        }
      },

      new Filter( "last", true ) {
        override def parameters = List( List(ArrayType) )

        override val invoke = {
          case List( l: List[_] ) => l.asInstanceOf[List[AnyRef]].last
        }
      },

      new Filter( "lstrip" ) {
        override def parameters = List( List(StringType) )

        override val invoke = {
          case List( s: String ) => s dropWhile (_.isWhitespace)
        }
      },

      new Filter( "map" ) {
        override def parameters = List( List(ArrayType, StringType) )

        override val invoke = {
          case List( l: List[_], k: String ) =>
            l map {
              case m: Map[String, Any] =>
                m get k match {
                  case None => nil
                  case Some( v ) => v
                }
              case _ => nil
            }
        }
      },

      new Filter( "prepend" ) {
        override def parameters = List( List(StringType, StringType) )

        override val invoke = {
          case List( l: String, r: String ) => r + l
        }
      },

      new Filter( "reverse", true ) {
        override def parameters = List( List(ArrayType) )

        override val invoke = {
          case List( l: List[_] ) => l.asInstanceOf[List[Any]].reverse
        }
      },

      new NumericFilter( "size", true ) {
        override def parameters = List( List(ArrayType), List(StringType) )

        override val compute = {
          case List( l: List[_] ) => l.length
          case List( s: String ) => s.length
        }
      },

//      new Filter( "sort", true ) {
//        override def parameters = List( List(ArrayType) )
//
//        override val invoke = {
//          case List( l: List[_] ) => l.sortBy
//        }
//      },

      new Filter( "uniq", true ) {
        override def parameters = List( List(ArrayType) )

        override val invoke = {
          case List( l: List[_] ) => l.asInstanceOf[List[Any]].distinct
        }
      }

    ) map {f => (f.name, f)} toMap

}