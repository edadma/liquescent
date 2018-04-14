//@
package xyz.hyperreal.fluidic

import scala.util.parsing.combinator._


class FluidicParser extends RegexParsers {

  def source: Parser[SourceAST] = opt(elements) ^^ {
    case None => SourceAST( Nil )
    case Some( elems ) => SourceAST( elems )
  }

  def elements: Parser[List[ElementAST]] =
    element ^^ (List( _ )) |
    element ~ elements ^^ {case e ~ l => e :: l}

  def element: Parser[ElementAST] =
    log(text)("text") | liquidQbject

  def text: Parser[TextElementAST] = """.+(?=\{\{|\{%|)""".r ^^ TextElementAST

  def liquidQbject: Parser[ObjectElementAST] = "{{" ~> objectSyntax <~ "}}"

  def ident: Parser[String] = """[a-zA-Z]+\w*""".r

  def objectSyntax: Parser[ObjectElementAST] = primary ^^ ObjectElementAST

  def primary: Parser[ExpressionAST] =
    "\"" ~> """[^"]*""".r <~ "\"" ^^ StringAST |
    ident.+ ^^ VariableAST

  def apply( input: String ): SourceAST =
    parseAll( source, input ) match {
      case Success( result, _ ) => result
      case failure : NoSuccess => sys.error( failure.msg )
    }

}

trait AST

case class SourceAST( elems: List[ElementAST]) extends AST

trait ElementAST extends AST

case class TextElementAST( s: String ) extends ElementAST
case class ObjectElementAST( expr: ExpressionAST ) extends ElementAST

trait ExpressionAST extends AST
case class StringAST( s: String ) extends ExpressionAST
case class VariableAST( ids: List[String] ) extends ExpressionAST