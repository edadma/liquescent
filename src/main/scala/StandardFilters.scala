//@
package xyz.hyperreal.liquescent

import java.math.MathContext
import java.time.{LocalDate, ZonedDateTime}
import java.time.format.DateTimeFormatter
import java.util.regex.Matcher

import xyz.hyperreal.lia.Math
import xyz.hyperreal.strftime.Strftime

import scala.collection.mutable


object StandardFilters {

  val nonWordCharacterRegex = """([^\w _.?])"""r

  def escape( s: String ) =
    nonWordCharacterRegex.replaceSomeIn( s,
      { m =>
        val c = m group 1 head

        val entity =
          c match {
            case '<' => "lt"
            case '>' => "gt"
            case '&' => "amp"
            case '"' => "quot"
            case '\'' => "apos"
            case '¢' => "cent"
            case '£' => "pound"
            case '¥' => "yen"
            case '€' => "euro"
            case '©' => "copy"
            case '®' => "reg"
            case _ => s"#${c.toInt}"
          }

        Some(s"&$entity;")
      } )

  val map =
    List(

      new NumericFilter( "abs" ) {
        override def parameters = List( List(NumberType), List(StringType) )

        override def compute( interp: Interpreter, settings: Map[Symbol, Any], globals: mutable.Map[String, Any], args: List[Any], named: Map[String, Any], locals: Map[String, Any] ) =
          args match {
            case List( a: Number ) => Math.absFunction( a )
            case List( a: String ) =>
              if (integer( a ))
                Math.absFunction( BigInt(a) )
              else if (float( a ))
                Math.absFunction( BigDecimal(a) )
              else
                0
          }
      },

      new Filter( "append" ) {
        override def parameters = List( List(StringType, StringType) )

        override def apply( interp: Interpreter, settings: Map[Symbol, Any], globals: mutable.Map[String, Any], args: List[Any], named: Map[String, Any], locals: Map[String, Any] ) =
          args match {
            case List( l: String, r: String ) => l + r
          }
      },

      new NumericFilter( "at_least" ) {
        override def parameters = List( List(NumberType, NumberType) )

        override def compute( interp: Interpreter, settings: Map[Symbol, Any], globals: mutable.Map[String, Any], args: List[Any], named: Map[String, Any], locals: Map[String, Any] ) =
          args match {
            case List( a: Number, b: Number ) => if (Math.predicate( Symbol("<"), a, b )) b else a
          }
      },

      new NumericFilter( "at_most" ) {
        override def parameters = List( List(NumberType, NumberType) )

        override def compute( interp: Interpreter, settings: Map[Symbol, Any], globals: mutable.Map[String, Any], args: List[Any], named: Map[String, Any], locals: Map[String, Any] ) =
          args match {
            case List( a: Number, b: Number ) => if (Math.predicate( Symbol(">"), a, b )) b else a
          }
      },

      new Filter( "capitalize" ) {
        override def parameters = List( List(StringType) )

        override def apply( interp: Interpreter, settings: Map[Symbol, Any], globals: mutable.Map[String, Any], args: List[Any], named: Map[String, Any], locals: Map[String, Any] ) =
          args match {
          case List( s: String ) => s"${s.head.toUpper}${s.tail.toLowerCase}"
        }
      },

      new NumericFilter( "ceil" ) {
        override def parameters = List( List(NumberType), List(StringType) )

        override def compute( interp: Interpreter, settings: Map[Symbol, Any], globals: mutable.Map[String, Any], args: List[Any], named: Map[String, Any], locals: Map[String, Any] ) =
          args match {
            case List( a: Number ) => Math.ceilFunction( a )
            case List( a: String ) =>
              if (integer( a ))
                Math.ceilFunction( BigInt(a) )
              else if (float( a ))
                Math.ceilFunction( BigDecimal(a) )
              else
                0
          }
      },

      new Filter( "compact", true ) {
        override def parameters = List( List(ArrayType) )

        override def apply( interp: Interpreter, settings: Map[Symbol, Any], globals: mutable.Map[String, Any], args: List[Any], named: Map[String, Any], locals: Map[String, Any] ) =
          args match {
            case List( l: Seq[_] ) => l filterNot (_ == nil)
          }
      },

      new Filter( "concat" ) {
        override def parameters = List( List(ArrayType, ArrayType) )

        override def apply( interp: Interpreter, settings: Map[Symbol, Any], globals: mutable.Map[String, Any], args: List[Any], named: Map[String, Any], locals: Map[String, Any] ) =
          args match {
            case List( l: Seq[_], r: Seq[_] ) => l ++ r
          }
      },

      new Filter( "date" ) {
        val ISO_DATETIME_REGEX = """\d\d\d\d-\d\d-\d\dT\d\d:\d\d:\d\d(?:.\d+)?(?:Z|(?:\+|-)\d\d:\d\d)""" r
        val USUAL_DATETIME_REGEX = """[a-zA-Z]+ \d+, \d+""" r
        val USUAL_DATETIME_FORMAT = DateTimeFormatter.ofPattern("MMMM dd, yyyy")
        val ISO_DATE_REGEX = """\d\d\d\d-\d\d-\d\d""" r
        val ISO_DATE_FORMAT = DateTimeFormatter.ofPattern("yyyy-MM-dd")

        override def parameters = List(List(StringType, StringType), List(TimestampType, StringType))

        override def apply( interp: Interpreter, settings: Map[Symbol, Any], globals: mutable.Map[String, Any], args: List[Any], named: Map[String, Any], locals: Map[String, Any] ) =
          args match {
            case List("now" | "today", f: String) => Strftime.format(f)
            case List(t: String, f: String) =>
              if (ISO_DATETIME_REGEX.pattern.matcher(t).matches)
                Strftime.format(f, ZonedDateTime.parse(t))
              else if (USUAL_DATETIME_REGEX.pattern.matcher(t).matches)
                Strftime.format(f, LocalDate.parse(t, USUAL_DATETIME_FORMAT))
              else if (ISO_DATE_REGEX.pattern.matcher(t).matches)
                Strftime.format(f, LocalDate.parse(t, ISO_DATE_FORMAT))
              else
                sys.error(s"unrecognized date/time format: $t")
          }
      },

      new Filter( "default" ) {
        override def parameters = List( List(AnyType, AnyType) )

        override def apply( interp: Interpreter, settings: Map[Symbol, Any], globals: mutable.Map[String, Any], args: List[Any], named: Map[String, Any], locals: Map[String, Any] ) =
          args match {
            case List( a: Any, b: Any ) => if (truthy( a ) && a != "") a else b
          }
      },

      new NumericFilter( "divided_by" ) {
        override def parameters = List( List(NumberType, NumberType) )

        override def compute( interp: Interpreter, settings: Map[Symbol, Any], globals: mutable.Map[String, Any], args: List[Any], named: Map[String, Any], locals: Map[String, Any] ) =
          args match {
            case List( a: Number, b: Number ) =>
              val quo = Math( Symbol("/"), a, b )

              if (integer( b ))
                Math.floorFunction( quo )
              else
                quo.asInstanceOf[Number]
          }
      },

      new Filter( "downcase" ) {
        override def parameters = List( List(StringType) )

        override def apply( interp: Interpreter, settings: Map[Symbol, Any], globals: mutable.Map[String, Any], args: List[Any], named: Map[String, Any], locals: Map[String, Any] ) =
          args match {
            case List( s: String ) => s.toLowerCase
          }
      },

      new Filter( "escape" ) {
        override def parameters = List( List(StringType) )

        override def apply( interp: Interpreter, settings: Map[Symbol, Any], globals: mutable.Map[String, Any], args: List[Any], named: Map[String, Any], locals: Map[String, Any] ) =
          args match {
            case List( s: String ) => escape( s )
          }
      },

      new Filter( "escape_once" ) {
        override def parameters = List( List(StringType) )

        val regex = """&#?\w+;"""r

        override def apply( interp: Interpreter, settings: Map[Symbol, Any], globals: mutable.Map[String, Any], args: List[Any], named: Map[String, Any], locals: Map[String, Any] ) =
          args match {
            case List( s: String ) =>
              val it = regex.findAllIn( s )
              var last = 0
              val buf = new StringBuilder

              while (it hasNext) {
                val m = it.next

                buf ++= escape( s.substring(last, it.start) )
                buf ++= it.matched
                last = it.end
              }

              buf ++= escape( s.substring(last, s.length) )
              buf.toString
          }
      },

      new Filter( "first", true ) {
        override def parameters = List( List(ArrayType) )

        override def apply( interp: Interpreter, settings: Map[Symbol, Any], globals: mutable.Map[String, Any], args: List[Any], named: Map[String, Any], locals: Map[String, Any] ) =
          args match {
            case List( l: Seq[_] ) => l.asInstanceOf[List[AnyRef]].head
          }
      },

      new NumericFilter( "floor" ) {
        override def parameters = List( List(NumberType), List(StringType) )

        override def compute( interp: Interpreter, settings: Map[Symbol, Any], globals: mutable.Map[String, Any], args: List[Any], named: Map[String, Any], locals: Map[String, Any] ) =
          args match {
            case List( a: Number ) => Math.floorFunction( a )
            case List( a: String ) =>
              if (integer( a ))
                Math.floorFunction( BigInt(a) )
              else if (float( a ))
                Math.floorFunction( BigDecimal(a) )
              else
                0
          }
      },

      new Filter( "join" ) {
        override def parameters = List( List(ArrayType, StringType) )

        override def apply( interp: Interpreter, settings: Map[Symbol, Any], globals: mutable.Map[String, Any], args: List[Any], named: Map[String, Any], locals: Map[String, Any] ) =
          args match {
            case List( l: Seq[_], s: String ) => l map display mkString s
          }
      },

      new Filter( "last", true ) {
        override def parameters = List( List(ArrayType) )

        override def apply( interp: Interpreter, settings: Map[Symbol, Any], globals: mutable.Map[String, Any], args: List[Any], named: Map[String, Any], locals: Map[String, Any] ) =
          args match {
            case List( l: Seq[_] ) => l.asInstanceOf[List[AnyRef]].last
          }
      },

      new Filter( "lstrip" ) {
        override def parameters = List( List(StringType) )

        override def apply( interp: Interpreter, settings: Map[Symbol, Any], globals: mutable.Map[String, Any], args: List[Any], named: Map[String, Any], locals: Map[String, Any] ) =
          args match {
            case List( s: String ) => s dropWhile (_.isWhitespace)
          }
      },

      new Filter( "map" ) {
        override def parameters = List( List(ArrayType, StringType) )

        override def apply( interp: Interpreter, settings: Map[Symbol, Any], globals: mutable.Map[String, Any], args: List[Any], named: Map[String, Any], locals: Map[String, Any] ) =
          args match {
            case List( l: Seq[_], k: String ) =>
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

      new NumericFilter( "minus" ) {
        override def parameters = List( List(NumberType, NumberType) )

        override def compute( interp: Interpreter, settings: Map[Symbol, Any], globals: mutable.Map[String, Any], args: List[Any], named: Map[String, Any], locals: Map[String, Any] ) =
          args match {
            case List( a: Number, b: Number ) => Math( Symbol("-"), a, b ).asInstanceOf[Number]
          }
      },

      new NumericFilter( "modulo" ) {
        override def parameters = List( List(NumberType, NumberType) )

        override def compute( interp: Interpreter, settings: Map[Symbol, Any], globals: mutable.Map[String, Any], args: List[Any], named: Map[String, Any], locals: Map[String, Any] ) =
          args match {
            case List( a: Number, b: Number ) => Math( Symbol("%"), a, b ).asInstanceOf[Number]
          }
      },

      new Filter( "newline_to_br" ) {
        override def parameters = List( List(StringType) )

        override def apply( interp: Interpreter, settings: Map[Symbol, Any], globals: mutable.Map[String, Any], args: List[Any], named: Map[String, Any], locals: Map[String, Any] ) =
          args match {
            case List( s: String ) => s replaceAll ("\r?\n", "<br />")
          }
      },

      new NumericFilter( "plus" ) {
        override def parameters = List( List(NumberType, NumberType) )

        override def compute( interp: Interpreter, settings: Map[Symbol, Any], globals: mutable.Map[String, Any], args: List[Any], named: Map[String, Any], locals: Map[String, Any] ) =
          args match {
            case List( a: Number, b: Number ) => Math( Symbol("+"), a, b ).asInstanceOf[Number]
          }
      },

      new Filter( "remove" ) {
        override def parameters = List( List(StringType, StringType) )

        override def apply( interp: Interpreter, settings: Map[Symbol, Any], globals: mutable.Map[String, Any], args: List[Any], named: Map[String, Any], locals: Map[String, Any] ) =
          args match {
            case List( l: String, r: String ) => l replace (r, "")
          }
      },

      new Filter( "remove_first" ) {
        override def parameters = List( List(StringType, StringType) )

        override def apply( interp: Interpreter, settings: Map[Symbol, Any], globals: mutable.Map[String, Any], args: List[Any], named: Map[String, Any], locals: Map[String, Any] ) =
          args match {
            case List( l: String, r: String ) => l replaceFirst (Matcher.quoteReplacement(r), "")
          }
      },

      new Filter( "replace" ) {
        override def parameters = List( List(StringType, StringType, StringType) )

        override def apply( interp: Interpreter, settings: Map[Symbol, Any], globals: mutable.Map[String, Any], args: List[Any], named: Map[String, Any], locals: Map[String, Any] ) =
          args match {
            case List( l: String, r1: String, r2: String ) => l replace (r1, r2)
          }
      },

      new Filter( "replace_first" ) {
        override def parameters = List( List(StringType, StringType, StringType) )

        override def apply( interp: Interpreter, settings: Map[Symbol, Any], globals: mutable.Map[String, Any], args: List[Any], named: Map[String, Any], locals: Map[String, Any] ) =
          args match {
            case List( l: String, r1: String, r2: String ) => l replaceFirst (Matcher.quoteReplacement(r1), r2)
          }
      },

      new Filter( "prepend" ) {
        override def parameters = List( List(StringType, StringType) )

        override def apply( interp: Interpreter, settings: Map[Symbol, Any], globals: mutable.Map[String, Any], args: List[Any], named: Map[String, Any], locals: Map[String, Any] ) =
          args match {
            case List( l: String, r: String ) => r + l
          }
      },

      new Filter( "reverse", true ) {
        override def parameters = List( List(ArrayType) )

        override def apply( interp: Interpreter, settings: Map[Symbol, Any], globals: mutable.Map[String, Any], args: List[Any], named: Map[String, Any], locals: Map[String, Any] ) =
          args match {
            case List( l: Seq[_] ) => l.asInstanceOf[Seq[Any]].reverse
          }
      },

      new NumericFilter( "round", false ) {
        override def parameters = List( List(NumberType), List(NumberType, NumberType) )

        override def compute( interp: Interpreter, settings: Map[Symbol, Any], globals: mutable.Map[String, Any], args: List[Any], named: Map[String, Any], locals: Map[String, Any] ) =
          args match {
            case List( n: Number ) if integer( n ) => n
            case List( n: Number, _ ) if integer( n ) => n
            case List( n: BigDecimal ) => round( n, 0, settings )
            case List( n: BigDecimal, scale: Int ) => round( n, scale, settings )
          }
      },

      new Filter( "rstrip" ) {
        override def parameters = List( List(StringType) )

        override def apply( interp: Interpreter, settings: Map[Symbol, Any], globals: mutable.Map[String, Any], args: List[Any], named: Map[String, Any], locals: Map[String, Any] ) =
          args match {
            case List( s: String ) => s.reverse dropWhile (_.isWhitespace) reverse
          }
      },

      new NumericFilter( "size", true ) {
        override def parameters = List( List(ArrayType), List(StringType) )

        override def compute( interp: Interpreter, settings: Map[Symbol, Any], globals: mutable.Map[String, Any], args: List[Any], named: Map[String, Any], locals: Map[String, Any] ) =
          args match {
            case List( l: Seq[_] ) => l.length
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

        override def apply( interp: Interpreter, settings: Map[Symbol, Any], globals: mutable.Map[String, Any], args: List[Any], named: Map[String, Any], locals: Map[String, Any] ) =
          args match {
            case List( s: String, idx: Number ) => s(index(s, idx.intValue)) toString
            case List( s: String, idx: Number, len: Number ) =>
              val i = index( s, idx.intValue )

              s slice (i, i + len.intValue)
         }
      },

      new Filter( "sort", true ) {
        override def parameters = List( List(ArrayType) )

        def lt( a: Any, b: Any ) =
          if (a.isInstanceOf[Number] && b.isInstanceOf[Number])
            Math.predicate( Symbol("<"), a, b )
          else
            a.toString < b.toString

        override def apply( interp: Interpreter, settings: Map[Symbol, Any], globals: mutable.Map[String, Any], args: List[Any], named: Map[String, Any], locals: Map[String, Any] ) =
          args match {
            case List( l: Seq[_] ) => l sortWith lt
          }
      },

      new Filter( "sort_natural", true ) {
        override def parameters = List( List(ArrayType) )

        def lt( a: Any, b: Any ) =
          if (a.isInstanceOf[Number] && b.isInstanceOf[Number])
            Math.predicate( Symbol("<"), a, b )
          else
            (a.toString compareToIgnoreCase b.toString) < 0

        override def apply( interp: Interpreter, settings: Map[Symbol, Any], globals: mutable.Map[String, Any], args: List[Any], named: Map[String, Any], locals: Map[String, Any] ) =
          args match {
            case List( l: Seq[_] ) => l sortWith lt
          }
      },

      new Filter( "split" ) {
        override def parameters = List( List(StringType, StringType) )

        override def apply( interp: Interpreter, settings: Map[Symbol, Any], globals: mutable.Map[String, Any], args: List[Any], named: Map[String, Any], locals: Map[String, Any] ) =
          args match {
            case List( l: String, r: String ) => l split Matcher.quoteReplacement(r) toList
          }
      },

      new Filter( "strip" ) {
        override def parameters = List( List(StringType) )

        override def apply( interp: Interpreter, settings: Map[Symbol, Any], globals: mutable.Map[String, Any], args: List[Any], named: Map[String, Any], locals: Map[String, Any] ) =
          args match {
            case List( s: String ) => s.trim
          }
      },

      new Filter( "strip_html" ) {
        override def parameters = List( List(StringType) )

        override def apply( interp: Interpreter, settings: Map[Symbol, Any], globals: mutable.Map[String, Any], args: List[Any], named: Map[String, Any], locals: Map[String, Any] ) =
          args match {
            case List( s: String ) => s replaceAll ("""</?\w+((\s+\w+(\s*=\s*(?:".*?"|'.*?'|[\^'">\s]+))?)+\s*|\s*)/?>""", "")
          }
      },

      new Filter( "strip_newlines" ) {
        override def parameters = List( List(StringType) )

        override def apply( interp: Interpreter, settings: Map[Symbol, Any], globals: mutable.Map[String, Any], args: List[Any], named: Map[String, Any], locals: Map[String, Any] ) =
          args match {
            case List( s: String ) => s replaceAll ("\r?\n", "")
          }
      },

      new NumericFilter( "times" ) {
        override def parameters = List( List(NumberType, NumberType) )

        override def compute( interp: Interpreter, settings: Map[Symbol, Any], globals: mutable.Map[String, Any], args: List[Any], named: Map[String, Any], locals: Map[String, Any] ) =
          args match {
            case List( a: Number, b: Number ) => Math( Symbol("*"), a, b ).asInstanceOf[Number]
          }
      },

      new Filter( "truncate" ) {
        override def parameters = List( List(StringType, NumberType), List(StringType, NumberType, StringType) )

        def truncate( s: String, len: Int, ellipsis: String ) =
          if (s.length <= len)
            s
          else
            s.substring( 0, len - ellipsis.length ) + ellipsis

        override def apply( interp: Interpreter, settings: Map[Symbol, Any], globals: mutable.Map[String, Any], args: List[Any], named: Map[String, Any], locals: Map[String, Any] ) =
          args match {
            case List( s: String, n: Number ) => truncate( s, n.intValue, "..." )
            case List( s: String, n: Number, ellipsis: String ) => truncate( s, n.intValue, ellipsis )
          }
      },

      new Filter( "truncatewords" ) {
        override def parameters = List( List(StringType, NumberType), List(StringType, NumberType, StringType) )

        def truncate( s: String, len: Int, ellipsis: String ) = {
          val words = s split """\s+"""

          if (words.length <= len)
            s
          else {
            val it = """\w+""".r.findAllIn( s )

            for (_ <- 1 to len)
              it.next

            s.substring( 0, it.end ) + ellipsis
          }
        }

        override def apply( interp: Interpreter, settings: Map[Symbol, Any], globals: mutable.Map[String, Any], args: List[Any], named: Map[String, Any], locals: Map[String, Any] ) =
          args match {
            case List( s: String, n: Number ) => truncate( s, n.intValue, "..." )
            case List( s: String, n: Number, ellipsis: String ) => truncate( s, n.intValue, ellipsis )
          }
      },

      new Filter( "uniq", true ) {
        override def parameters = List( List(ArrayType) )

        override def apply( interp: Interpreter, settings: Map[Symbol, Any], globals: mutable.Map[String, Any], args: List[Any], named: Map[String, Any], locals: Map[String, Any] ) =
          args match {
            case List( l: Seq[_] ) => l.asInstanceOf[List[Any]].distinct
          }
      }

    ) map {f => (f.name, f)} toMap

}