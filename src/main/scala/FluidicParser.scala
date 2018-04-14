//@
package xyz.hyperreal.fluidic

import scala.util.parsing.combinator._


class FluidicParser extends RegexParsers {

  def text: Parser[Text] = """.*?"""

}

trait AST

case class Text( s: String ) extends AST