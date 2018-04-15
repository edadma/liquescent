//@
package xyz.hyperreal.fluidic

import scala.collection.mutable.ListBuffer
import scala.util.parsing.combinator._


object FluidicParser {

//  val objectRegex = """\{\{.*}}"""r
//  val tagRegex = """\{%.*%}"""r
//  val textBeforeElementRegex = """.+?(?=\{\{|\{%)"""r
//  val textRegex = """.*"""r

  def templateRegex = """\{\{.*?}}|\{%.*?%}"""r

  def elements( src: String ): List[Element] = {
    val buf = new ListBuffer[Element]
    val it = templateRegex.findAllIn( src )
    var after = 0

    while (it.hasNext) {
      it.next

      if (it.start != after)
        buf += TextElement( src.substring(after, it.start) )

      buf +=
        (src.charAt( it.start + 1 ) match {
          case '{' => ObjectElement( it.matched.substring(2, it.matched.length - 2) )
          case '%' => TagElement( it.matched.substring(2, it.matched.length - 2) )
        })
      after = it.end
    }

    if (after != src.length)
      buf += TextElement( src.substring(after, src.length) )

    buf.toList
  }

}

trait Element

case class TextElement( text: String ) extends Element
case class ObjectElement( expr: String ) extends Element
case class TagElement( tag: String ) extends Element

class FluidicParser extends RegexParsers {

  def source: Parser[SourceAST] = opt(elements) ^^ {
    case None => SourceAST( Nil )
    case Some( elems ) => SourceAST( elems )
  }

  def elements: Parser[List[ElementAST]] =
    element.+
//    element ~ elements ^^ {case e ~ l => e :: l}

  def element: Parser[ElementAST] =
    log(liquidQbject)("object") | log(text)("text")

  def text: Parser[TextElementAST] = """.+?(?=\{\{|\{%|\z)""".r ^^ TextElementAST

  def liquidQbject: Parser[ObjectElementAST] = "{{" ~> objectSyntax <~ "}}"

  def ident: Parser[String] = """[a-zA-Z]+\w*""".r

  def objectSyntax: Parser[ObjectElementAST] = primary ^^ ObjectElementAST

  def primary: Parser[ExpressionAST] =
    "\"" ~> """[^"]*""".r <~ "\"" ^^ StringAST |
    rep1sep(ident, ".") ^^ VariableAST

  def apply( input: String ) =
    parseAll( source, input ) match {
      case Success( result, _ ) => result
      case NoSuccess( msg, r ) => s"$msg (${r.pos})\n${r.pos.longString}"
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