package xyz.hyperreal.liquescent

import java.io.File
import java.nio

import scala.collection.mutable

import xyz.hyperreal.args.Options


object Main extends App {

	val assigns = new mutable.HashMap[String, Any]
	var template: File = _

	def usage {
		"""
			|liquescent v0.1
			|
			|Usage:  java -jar liquescent-0.1.jar <options> <liquid template>
			|
			|Options:  --help              display this help and exit
			|          -s <name> <string>  assign <string> to variable <name>
			|          -n <name> <number>  assign <number> to variable <name>
			|
			|Note:  <liquid template> may be -- meaning read from standard input
		""".trim.stripMargin.lines foreach println
		sys.exit
	}

	if (args isEmpty)
		usage

	Options( args ) {
		case List() =>
			println( 123 )
			Nil
		case "-s" :: name :: s :: t =>
			assigns(name) = s
			t
		case "-n" :: name :: n :: t =>
			number( n ) match {
				case None => sys.error( s"not a number: $n" )
				case Some( v ) => assigns(name) = v
			}

			t
		case "--help" :: _ =>
			usage
			Nil
		case "--" :: Nil =>
			new Interpreter(StandardFilters.map, assigns toMap ).perform( LiquescentParser.parse(io.Source.stdin.mkString), Console.out )
			Nil
		case file :: Nil =>
			template = new File( file )

			if (template.exists && template.isFile && template.canRead) {
				new Interpreter(StandardFilters.map, assigns toMap ).perform( LiquescentParser.parse(io.Source.fromFile(template).mkString), Console.out )
				Nil
			} else
				sys.error( s"error reading file: $file" )
	}

}