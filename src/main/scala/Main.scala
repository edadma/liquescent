package xyz.hyperreal.liquescent

import java.io.File
import java.nio.charset.Charset

import scala.collection.mutable
import xyz.hyperreal.args.Options
import xyz.hyperreal.json.{DefaultJSONReader, JSON}


object Main extends App {

	val assigns = new mutable.HashMap[String, Any]
	var templateFile: File = _

	def usage: Unit = {
		println(
			"""
				|liquescent v0.3
				|
				|Usage:  java -jar liquescent-0.3.jar <options> <liquid template>
				|
				|Options:  --help              display this help and exit
				|          -s <name> <string>  assign <string> to variable <name>
				|          -n <name> <number>  assign <number> to variable <name>
				|
				|Note:  <liquid template> may be -- meaning read from standard input
			""" )
		sys.exit
	}

	def json( src: io.Source ) =
		for ((k: String, v) <- DefaultJSONReader.fromString( src mkString ).asInstanceOf[JSON])
			assigns(k) = v

	if (args isEmpty)
		usage

	Options( args ) {
		case "-s" :: name :: s :: t =>
			assigns(name) = s
			t
		case "-n" :: name :: n :: t =>
			number( n ) match {
				case None => sys.error( s"not a number: $n" )
				case Some( v ) => assigns(name) = v
			}

			t
		case "-j" :: "--" :: t =>
			json( io.Source.stdin )
			t
		case "-j" :: file :: t if !file.matches( """\s*\{.*""" ) =>
			val jsonFile = new File( file )

			if (jsonFile.exists && jsonFile.isFile && jsonFile.canRead) {
				json( io.Source.fromFile(jsonFile) )
			} else
				sys.error( s"error reading file: $file" )

			t
		case "-j" :: s :: t =>
			json( io.Source.fromString(s) )
			t
		case "--help" :: _ =>
			usage
			Nil
		case "--" :: Nil =>
			new Interpreter(StandardFilters.map ++ ExtraStringFilters.map ++ ExtraHTMLFilters.map, Map(), Map(), assigns toMap, null, Charset.defaultCharset ).
        render( LiquescentParser.parse(io.Source.stdin), Map(), Console.out, false )
			Nil
		case s :: _ if s startsWith "-" => sys.error( s"invalid switch $s" )
		case file :: Nil =>
			templateFile = new File( file )

			if (templateFile.exists && templateFile.isFile && templateFile.canRead) {
				new Interpreter(StandardFilters.map ++ ExtraStringFilters.map ++ ExtraHTMLFilters.map, Map(), Map(), assigns toMap, null, Charset.defaultCharset ).
          render( LiquescentParser.parse(io.Source.fromFile(templateFile)), Map(), Console.out, false )
				Nil
			} else
				sys.error( s"error reading file: $file" )
	}

}