//@
package xyz.hyperreal.liquescent

import scala.collection.mutable


object JekyllFilters {

  def concatPaths( a: String, b: String ) =
    if (a.endsWith( "/" ) && b.startsWith( "/" ))
      a + (b drop 1)
    else if (!a.endsWith( "/" ) && !b.startsWith( "/" ))
      s"$a/$b"
    else
      a + b

  val map =
    List(

      new Filter( "where" ) {
        override def parameters = List( List(ArrayType, StringType, AnyType) )

        override def apply( interp: Interpreter, settings: Map[Symbol, Any], globals: mutable.Map[String, Any], args: List[Any], named: Map[String, Any], locals: Map[String, Any] ) =
          args match {
            case List( array: Seq[_], prop: String, v: Any ) =>
              array.filter {
                m =>
                  m.asInstanceOf[Map[String, Any]] get prop match {
                    case None => false
                    case Some( a ) => a == v
                  }
              }
          }
      },

      new Filter( "group_by" ) {
        override def parameters = List( List(ArrayType, StringType) )

        override def apply( interp: Interpreter, settings: Map[Symbol, Any], globals: mutable.Map[String, Any], args: List[Any], named: Map[String, Any], locals: Map[String, Any] ) =
          args match {
            case List( array: Seq[_], prop: String ) =>
              (array groupBy (_.asInstanceOf[Map[String, Any]] getOrElse (prop, null)) toList) map {case (k, v) => Map("name" -> k, "items" -> v)}
          }
      },

      new Filter( "relative_url" ) {
        override def parameters = List( List(StringType) )

        override def apply( interp: Interpreter, settings: Map[Symbol, Any], globals: mutable.Map[String, Any], args: List[Any], named: Map[String, Any], locals: Map[String, Any] ) =
          args match {
            case List( path: String ) =>
              concatPaths( interp.getVar("baseurl", locals).toString, path )
          }
      },

      new Filter( "absolute_url" ) {
        override def parameters = List( List(StringType) )

        override def apply( interp: Interpreter, settings: Map[Symbol, Any], globals: mutable.Map[String, Any], args: List[Any], named: Map[String, Any], locals: Map[String, Any] ) =
          args match {
            case List( path: String ) =>
              concatPaths( interp.getVar("url", locals).toString, concatPaths(globals("baseurl").toString, path) )
          }
      },

      new Filter( "array_to_sentence_string" ) {
        override def parameters = List( List(ArrayType), List(ArrayType, StringType) )

        def string( l: Seq[Any], connector: String ) =
          l match {
            case Nil => ""
            case Seq( a ) => a.toString
            case Seq( a, b ) => s"$a $connector $b"
            case _ => s"${l.init mkString ", "}, $connector ${l.last}"
          }

        override def apply( interp: Interpreter, settings: Map[Symbol, Any], globals: mutable.Map[String, Any], args: List[Any], named: Map[String, Any], locals: Map[String, Any] ) =
          args match {
            case List( l: Seq[Any] ) => string( l, "and" )
            case List( l: Seq[Any], connector: String ) => string( l, connector )
          }
      },

      new Filter( "sample" ) {
        override def parameters = List( List(ArrayType), List(ArrayType, NumberType) )

        override def apply( interp: Interpreter, settings: Map[Symbol, Any], globals: mutable.Map[String, Any], args: List[Any], named: Map[String, Any], locals: Map[String, Any] ) =
          args match {
            case List( l: Seq[Any] ) => l( util.Random.nextInt(l.length) )
            case List( l: Seq[Any], n: Number ) => util.Random.shuffle( l ) take n.intValue
          }
      },

      new Filter( "to_integer" ) {
        override def parameters = List( List(StringType) )

        override def apply( interp: Interpreter, settings: Map[Symbol, Any], globals: mutable.Map[String, Any], args: List[Any], named: Map[String, Any], locals: Map[String, Any] ) =
          args match {
            case List( n: String ) =>
              number( n ) match {
                case None => sys.error( s"not a number: $n" )
                case Some( d: Double ) => d.toInt
                case Some( d: BigDecimal ) => d.toBigInt
                case Some( i ) => i
              }
          }
      },

      new Filter( "normalize_whitespace" ) {
        override def parameters = List( List(StringType) )

        override def apply( interp: Interpreter, settings: Map[Symbol, Any], globals: mutable.Map[String, Any], args: List[Any], named: Map[String, Any], locals: Map[String, Any] ) =
          args match {
            case List( s: String ) => s.replaceAll( """\s+""", " " )
          }
      },

      new Filter( "push" ) {
        override def parameters = List( List(ArrayType, AnyType) )

        override def apply( interp: Interpreter, settings: Map[Symbol, Any], globals: mutable.Map[String, Any], args: List[Any], named: Map[String, Any], locals: Map[String, Any] ) =
          args match {
            case List( l: Seq[Any], item: Any ) => l :+ item
          }
      },

      new Filter( "pop" ) {
        override def parameters = List( List(ArrayType) )

        override def apply( interp: Interpreter, settings: Map[Symbol, Any], globals: mutable.Map[String, Any], args: List[Any], named: Map[String, Any], locals: Map[String, Any] ) =
          args match {
            case List( l: Seq[Any] ) => l.last
          }
      },

      new Filter( "unshift" ) {
        override def parameters = List( List(ArrayType, AnyType) )

        override def apply( interp: Interpreter, settings: Map[Symbol, Any], globals: mutable.Map[String, Any], args: List[Any], named: Map[String, Any], locals: Map[String, Any] ) =
          args match {
            case List( l: Seq[Any], item: Any ) => item +: l
          }
      },

      new Filter( "shift" ) {
        override def parameters = List( List(ArrayType) )

        override def apply( interp: Interpreter, settings: Map[Symbol, Any], globals: mutable.Map[String, Any], args: List[Any], named: Map[String, Any], locals: Map[String, Any] ) =
          args match {
            case List( l: Seq[Any] ) => l.head
          }
      },

    ) map {f => (f.name, f)} toMap

}