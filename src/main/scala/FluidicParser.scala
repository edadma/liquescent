//@
package xyz.hyperreal.fluidic

import scala.collection.mutable.ListBuffer
import scala.util.parsing.combinator._
import scala.util.parsing.combinator.lexical.Lexical
import scala.util.parsing.combinator.syntactical.TokenParsers
import scala.util.parsing.combinator.token.Tokens


class FluidicLexical extends Lexical with Tokens {

  override def token: Parser[Token] = ???

}

class FluidicParser extends TokenParsers {

  override val lexical: Tokens = new FluidicLexical



}