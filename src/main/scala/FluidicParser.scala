//@
package xyz.hyperreal.fluidic

import java.util.Scanner
import scala.collection.mutable.ListBuffer
import scala.util.parsing.combinator._

object FluidicParser {

//  val objectRegex = """\{\{.*}}"""r
//  val tagRegex = """\{%.*%}"""r
//  val textBeforeElementRegex = """.+?(?=\{\{|\{%)"""r
//  val textRegex = """.*"""r

  val templateRegex = """\{\{.*?}}|\{%.*?%}"""r
  val delimiterPattern = """[\s%{}]+""".r.pattern
  val tagPattern = """[a-zA-Z]+[a-zA-Z0-9]*""".r.pattern

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
          case '{' => ObjectElement( it.matched )//.substring(2, it.matched.length - 2)
          case '%' =>
            val matched = it.matched
            val scanner = new Scanner( matched ) useDelimiter delimiterPattern

            TagElement( scanner.next(tagPattern), matched )
        })
      after = it.end
    }

    if (after != src.length)
      buf += TextElement( src.substring(after, src.length) )

    buf.toList
  }

  def parse( template: String ) = {
    elements( template ) filterNot new CommentFilter flatMap new RawTransform map {
      case TextElement( s ) => OutputAST( StringExpressionAST(s) )
      case ObjectElement( s ) =>
        val parser = new ObjectParser

        parser.parseAll( parser.objectGrammar, s ) match {
          case parser.Success( result, _ ) => result//.asInstanceOf[OutputAST]
          case parser.NoSuccess( msg, r ) =>
            println( s"$msg (${r.pos})\n${r.pos.longString}" )
            sys.exit( 1 )
        }
    }
  }

}

class CommentFilter extends (Element => Boolean) {
  var dropping = false

  def apply( elem: Element ) =
    elem match {
      case TagElement( "comment", _ ) if !dropping =>
        dropping = true
        true
      case TagElement( "endcomment", _ ) if dropping =>
        dropping = false
        true
      case _ => dropping
    }
}

class RawTransform extends (Element => Seq[Element]) {
  var raw = false

  def apply( elem: Element ) =
    elem match {
      case TagElement( "raw", _ ) if !raw =>
        raw = true
        Nil
      case TagElement( "endraw", _ ) if raw =>
        raw = false
        Nil
      case TagElement( _, s ) if raw => List( TextElement(s) )
      case ObjectElement( s ) if raw => List( TextElement(s) )
      case _ => List( elem )
    }
}

trait Element

case class TextElement( s: String ) extends Element
case class ObjectElement( s: String ) extends Element
case class TagElement( tag: String, s: String ) extends Element

class ObjectParser extends RegexParsers with PackratParsers {

//  def source: Parser[SourceAST] = opt(elements) ^^ {
//    case None => SourceAST( Nil )
//    case Some( elems ) => SourceAST( elems )
//  }
//
//  def elements: Parser[List[ElementAST]] =
//    element.+
////    element ~ elements ^^ {case e ~ l => e :: l}
//
//  def element: Parser[ElementAST] =
//    log(liquidQbject)("object") | log(text)("text")
//
//  def text: Parser[TextElementAST] = """.+?(?=\{\{|\{%|\z)""".r ^^ TextElementAST

  lazy val ident: Parser[String] = """[a-zA-Z]+\w*""".r

  lazy val objectGrammar: PackratParser[OutputAST] = "{{" ~> expression <~ "}}" ^^ OutputAST

  lazy val expression: PackratParser[ExpressionAST] = applyExpression

  lazy val applyExpression: PackratParser[ExpressionAST] =
    applyExpression ~ ("." ~> ident) ^^ { case e ~ n => ArrayExpressionAST( e, StringExpressionAST(n) ) } |
    applyExpression ~ ("[" ~> expression <~ "]") ^^ { case e ~ n => ArrayExpressionAST( e, n ) } |
    primaryExpression

  lazy val primaryExpression: Parser[ExpressionAST] =
    "\"" ~> """[^"]*""".r <~ "\"" ^^ StringExpressionAST |
    ident ^^ VariableExpressionAST |
    """\d+(\.\d*)?""".r ^^ { n => NumberExpressionAST( n.toDouble ) }

  def apply[T]( grammar: Parser[T], input: String ) =
    parseAll( grammar, input ) match {
      case Success( result, _ ) => result
      case NoSuccess( msg, r ) => s"$msg (${r.pos})\n${r.pos.longString}"
    }

}

trait AST

case class SourceAST( elems: List[OperationAST]) extends AST

trait OperationAST extends AST

case class OutputAST( expr: ExpressionAST ) extends OperationAST

trait ExpressionAST extends AST
case class ArrayExpressionAST( expr: ExpressionAST, name: ExpressionAST ) extends ExpressionAST
case class StringExpressionAST( s: String ) extends ExpressionAST
case class NumberExpressionAST( n: Double ) extends ExpressionAST
case class VariableExpressionAST( name: String ) extends ExpressionAST