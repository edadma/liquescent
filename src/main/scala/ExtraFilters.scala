//@
package xyz.hyperreal.liquescent

import xyz.hyperreal.json.{DefaultJSONReader, JSON}


object ExtraFilters {

  val map =
    List(

      new Filter( "t" ) {
        var locale: String = _
        var translations: JSON = _

        def translate( settings: Map[Symbol, Any], key: String, vars: Map[String, String] ) = {
          if (translations == null || locale != settings("locale")) {
            locale = settings("locale").toString
            translations = DefaultJSONReader.fromFile( docroot(s"locale/$locale", settings) )
          }

          translations getString key
        }

        override def parameters = List( List(StringType) )

        override def apply( settings: Map[Symbol, Any], args: List[Any], named: Map[String, Any] ) =
          args match {
            case List( s: String ) =>
          }
      }

    ) map {f => (f.name, f)} toMap

}