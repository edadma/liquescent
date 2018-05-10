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
            case List( array: List[_], prop: String, v: Any ) =>
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
            case List( array: List[_], prop: String ) =>
              (array groupBy (_.asInstanceOf[Map[String, Any]] getOrElse (prop, null)) toList) map {case (k, v) => Map("name" -> k, "items" -> v)}
          }
      },

      new Filter( "relative_url" ) {
        override def parameters = List( List(StringType) )

        override def apply( interp: Interpreter, settings: Map[Symbol, Any], globals: mutable.Map[String, Any], args: List[Any], named: Map[String, Any], locals: Map[String, Any] ) =
          args match {
            case List( path: String ) =>
              concatPaths( globals("baseurl").toString, path )
          }
      },

      new Filter( "absolute_url" ) {
        override def parameters = List( List(StringType) )

        override def apply( interp: Interpreter, settings: Map[Symbol, Any], globals: mutable.Map[String, Any], args: List[Any], named: Map[String, Any], locals: Map[String, Any] ) =
          args match {
            case List( path: String ) =>
              concatPaths( globals("url").toString, concatPaths(globals("baseurl").toString, path) )
          }
      }

    ) map {f => (f.name, f)} toMap

}