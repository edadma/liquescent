//@
package xyz.hyperreal.liquescent

import java.io.PrintStream

import scala.collection.mutable


abstract class Tag( val name: String ) extends ((Map[Symbol, Any], mutable.Map[String, Any], PrintStream, List[Any], AnyRef) => Unit)

object Tag {

	def apply( tags: Tag* ) = tags map (t => t.name -> t) toMap

}