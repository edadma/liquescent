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
          if (translations == null || locale != settings('locale)) {
            locale = settings('locale).toString
            translations = DefaultJSONReader.fromFile( docroot(s"locales/$locale.json", settings) )
          }

          def traverse( k: List[String], obj: AnyRef ): String =
            k match {
              case Nil => obj.toString
              case h :: t => traverse( t, obj.asInstanceOf[Map[String, AnyRef]](h) )
            }

          traverse( key split "\\." toList, translations )
        }

        override def parameters = List( List(StringType) )

        override def apply( interp: Interpreter, settings: Map[Symbol, Any], args: List[Any], named: Map[String, Any], locals: Map[String, String] ) =
          args match {
            case List( key: String ) => interp.capture( LiquescentParser.parse(io.Source.fromString(translate(settings, key, named.asInstanceOf[Map[String, String]]))).statement, locals )
          }
      }

    ) map {f => (f.name, f)} toMap

}