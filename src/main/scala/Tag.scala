//@
package xyz.hyperreal.liquescent
import java.io.PrintStream

import scala.collection.mutable


abstract class Tag( val name: String ) {

	def apply( vars: mutable.Map[String, Any], out: PrintStream, args: List[Any] )

}

object Tag {

	def map( tags: Tag* ) = tags map (t => (t.name -> t)) toMap

}