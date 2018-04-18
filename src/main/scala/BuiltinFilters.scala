//@
package xyz.hyperreal.fluidic

import java.time.{LocalDate, LocalDateTime}
import java.time.format.DateTimeFormatter
import java.util.regex.Matcher

import math._
import xyz.hyperreal.strftime.Strftime


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
              case m: collection.Map[_, _] =>
                m.asInstanceOf[collection.Map[String, Any]] get k match {
                  case None => nil
                  case Some( v ) => v
                }
              case _ => nil
            }
        }
      },

      new Filter( "newline_to_br" ) {
        override def parameters = List( List(StringType) )

        override val invoke = {
          case List( s: String ) => s replace ("\n", "<br>")
        }
      },

      new Filter( "remove" ) {
        override def parameters = List( List(StringType, StringType) )

        override val invoke = {
          case List( l: String, r: String ) => l replace (r, "")
        }
      },

      new Filter( "remove_first" ) {
        override def parameters = List( List(StringType, StringType) )

        override val invoke = {
          case List( l: String, r: String ) => l replaceFirst (Matcher.quoteReplacement(r), "")
        }
      },

      new Filter( "replace" ) {
        override def parameters = List( List(StringType, StringType, StringType) )

        override val invoke = {
          case List( l: String, r1: String, r2: String ) => l replace (r1, r2)
        }
      },

      new Filter( "replace_first" ) {
        override def parameters = List( List(StringType, StringType, StringType) )

        override val invoke = {
          case List( l: String, r1: String, r2: String ) => l replaceFirst (Matcher.quoteReplacement(r1), r2)
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

      new Filter( "rstrip" ) {
        override def parameters = List( List(StringType) )

        override val invoke = {
          case List( s: String ) => s.reverse dropWhile (_.isWhitespace) reverse
        }
      },

      new NumericFilter( "size", true ) {
        override def parameters = List( List(ArrayType), List(StringType) )

        override val compute = {
          case List( l: List[_] ) => l.length
          case List( s: String ) => s.length
        }
      },

      new Filter( "slice" ) {
        override def parameters = List( List(StringType, NumberType), List(StringType, NumberType, NumberType) )

        def index( s: String, idx: Int ) =
          if (idx < 0)
            s.length + idx
          else
            idx

        override val invoke = {
          case List( s: String, idx: Number ) => s(index(s, idx.intValue)) toString
          case List( s: String, idx: Number, len: Number ) =>
            val i = index( s, idx.intValue )

            s slice (i, i + len.intValue)
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