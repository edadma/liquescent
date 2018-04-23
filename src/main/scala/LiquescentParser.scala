//@
package xyz.hyperreal.liquescent

import java.util.Scanner
import scala.collection.mutable.ListBuffer
import scala.util.parsing.combinator._


object LiquescentParser {

//  val objectRegex = """\{\{.*}}"""r
//  val tagRegex = """\{%.*%}"""r
//  val textBeforeElementRegex = """.+?(?=\{\{|\{%)"""r
//  val textRegex = """.*"""r

  val templateRegex = """\{\{.*?}}|\{%.*?%}"""r
  val delimiterPattern = """[\s%{}-]+""".r.pattern
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
          case '{' => ObjectElement( it.matched )
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

  def whitespace( elems: List[Element] ): List[Element] =
    elems match {
      case TextElement( pre ) :: (t@TagElement( _, tag )) :: tail if tag.startsWith( "{%-" ) =>
        if (pre.forall( _.isWhitespace ))
          whitespace( t :: tail )
        else
          whitespace( TextElement(pre.reverse dropWhile (_.isWhitespace) reverse) :: t :: tail )
      case (t@TagElement( _, tag )) :: TextElement( post ) :: tail if tag.endsWith( "-%}" ) =>
        if (post.forall( _.isWhitespace ))
          whitespace( t :: tail )
        else
          whitespace( t :: TextElement( post dropWhile (_.isWhitespace) ) :: tail )
     case TextElement( pre ) :: (o@ObjectElement( obj )) :: tail if obj.startsWith( "{{-" ) =>
        if (pre.forall( _.isWhitespace ))
          whitespace( o :: tail )
        else
          whitespace( TextElement(pre.reverse dropWhile (_.isWhitespace) reverse) :: o :: tail )
      case (o@ObjectElement( obj )) :: TextElement( post ) :: tail if obj.endsWith( "-}}" ) =>
        if (post.forall( _.isWhitespace ))
          whitespace( o :: tail )
        else
          whitespace( o :: TextElement( post dropWhile (_.isWhitespace) ) :: tail )
      case head :: tail => head :: whitespace( tail )
      case Nil => Nil
   }

  def parse( template: String ) = {
    var tokens = whitespace( elements(template) filterNot new CommentFilter flatMap new RawTransform )

    def peek = tokens.head

    def token( tok: String ) =
      peek match {
        case TagElement( `tok`, _ ) => true
        case _ => false
      }

    def tokenAdvance( tok: String ) =
      if (token( tok )) {
        advance
        true
      } else
        false

    def pop = {
      val t = peek

      advance
      t
    }

    def popTag = pop.asInstanceOf[TagElement]

    def advance = tokens = tokens.tail

    def consume( tok: String ) =
      if (eoi) {
        sys.error( s" expected '$tok' tag, but end of input encountered" )
      } else if (!tokenAdvance( tok )) {
        sys.error( s" expected '$tok' tag, but '$peek' encountered" )
      }

    def eoi = tokens == Nil

    def parseIf( s: String ) = {
      val parser = new ElementParser
      val cond = parser( parser.ifTag, s )
      val conds = new ListBuffer[(ExpressionAST, StatementAST)]

      conds += cond -> parseBlock

      while (token( "elsif" )) {
        val parser = new ElementParser
        val cond = parser( parser.elsifTag, popTag.s )

        conds += cond -> parseBlock
      }

      val els =
        if (tokenAdvance( "else" ))
          Some( parseBlock )
        else
          None

      consume( "endif" )
      IfStatementAST( conds.toList, els )
    }

    def parseUnless( s: String ) = {
      val parser = new ElementParser
      val cond = parser( parser.unlessTag, s )
      val conds = new ListBuffer[(ExpressionAST, StatementAST)]

      conds += cond -> parseBlock

      while (token( "elsif" )) {
        val parser = new ElementParser
        val cond = parser( parser.elsifTag, popTag.s )

        conds += cond -> parseBlock
      }

      val els =
        if (tokenAdvance( "else" ))
          Some( parseBlock )
        else
          None

      consume( "endunless" )
      UnlessStatementAST( conds.toList, els )
    }

    def parseCase( s: String ) = {
      val parser = new ElementParser
      val expr = parser( parser.caseTag, s )
      val cases = new ListBuffer[(ExpressionAST, StatementAST)]

      parseBlock

      while (token( "when" )) {
        val parser = new ElementParser
        val when = parser( parser.whenTag, popTag.s )

        cases += when -> parseBlock
      }

      val els =
        if (tokenAdvance( "else" ))
          Some( parseBlock )
        else
          None

      consume( "endcase" )
      CaseStatementAST( expr, cases.toList, els )
    }

    def parseFor( s: String ) = {
      val parser = new ElementParser
      val ForGenerator( name, expr, parameters ) = parser( parser.forTag, s )
			val body = parseBlock

      consume( "endfor" )
      ForStatementAST( name, expr, parameters, body )
    }

    def parseBlock: StatementAST = {
      val block = new ListBuffer[StatementAST]

      def _parseBlock: Unit =
        if (!eoi) {
          peek match {
            case TextElement( s ) =>
              advance
              block += PlainOutputStatementAST( s )
              _parseBlock
            case ObjectElement( s ) =>
              val parser = new ElementParser

              advance
              block += parser( parser.objectOutput, s )
              _parseBlock
            case TagElement( "break", _ ) =>
              advance
              block += BreakStatementAST
              _parseBlock
            case TagElement( "continue", _ ) =>
              advance
              block += ContinueStatementAST
              _parseBlock
            case TagElement( "cycle", s ) =>
              val parser = new ElementParser

              advance
              block += parser( parser.cycleTag, s )
              _parseBlock
            case TagElement( "increment", s ) =>
              val parser = new ElementParser

              advance
              block += parser( parser.incrementTag, s )
              _parseBlock
             case TagElement( "decrement", s ) =>
              val parser = new ElementParser

              advance
              block += parser( parser.decrementTag, s )
              _parseBlock
           case TagElement( "if", s ) =>
              advance
              block += parseIf( s )
              _parseBlock
            case TagElement( "unless", s ) =>
              advance
              block += parseUnless( s )
              _parseBlock
            case TagElement( "case", s ) =>
              advance
              block += parseCase( s )
              _parseBlock
            case TagElement( "for", s ) =>
              advance
              block += parseFor( s )
              _parseBlock
            case TagElement( "assign", s ) =>
              val parser = new ElementParser

              advance
              block += parser( parser.assignTag, s )
              _parseBlock
            case TagElement( "capture", s ) =>
              val parser = new ElementParser

              advance
              block += CaptureStatementAST( parser(parser.captureTag, s), parseBlock )
              consume( "endcapture" )
              _parseBlock
            case TagElement( "endif"|"endfor"|"endcase"|"endunless"|"endtablerow"|"endcapture"|"else"|"elsif"|"when", _ ) =>
          }
        }

      _parseBlock

      if (block.length == 1)
        block.head
      else
        BlockStatementAST( block toList )
    }

    val block = parseBlock

    if (!eoi)
      sys.error( s"unexpected element $pop" )

    block
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

class ElementParser extends RegexParsers with PackratParsers {

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

//  lazy val tagGrammar: PackratParser[StatementAST] = "{%" ~> tags <~ "%}"
//
//  lazy val tags: PackratParser[StatementAST] =
//    ifTag

  lazy val tagStart = "\\{%-?"r

  lazy val tagEnd = "-?%}"r

  lazy val assignTag: PackratParser[StatementAST] = tagStart ~> "assign" ~> ((ident <~ "=") ~ expression) <~ tagEnd ^^ {
    case n ~ e => AssignStatementAST( n, e ) }

  lazy val cycleTag: PackratParser[StatementAST] = tagStart ~> "cycle" ~> rep1sep(expression, ",") <~ tagEnd ^^ { xs => CycleStatementAST( xs.toVector ) }

  lazy val captureTag: PackratParser[String] = tagStart ~> "capture" ~> ident <~ tagEnd

  lazy val forTag: PackratParser[ForGenerator] = tagStart ~> "for" ~> ((ident <~ "in") ~ expression) ~ rep(forParameters) <~ tagEnd ^^ {
    case n ~ e ~ p => ForGenerator( n, e, p ) }

  lazy val forParameters: PackratParser[ForParameter] =
    "reversed" ^^^ ReversedForParameter |
    "offset" ~> ":" ~> expression ^^ OffsetForParameter |
    "limit" ~> ":" ~> expression ^^ LimitForParameter

  lazy val incrementTag: PackratParser[IncrementStatementAST] = tagStart ~> "increment" ~> ident <~ tagEnd ^^ IncrementStatementAST

  lazy val decrementTag: PackratParser[DecrementStatementAST] = tagStart ~> "decrement" ~> ident <~ tagEnd ^^ DecrementStatementAST

  lazy val ifTag: PackratParser[ExpressionAST] = tagStart ~> "if" ~> expression <~ tagEnd

  lazy val unlessTag: PackratParser[ExpressionAST] = tagStart ~> "unless" ~> expression <~ tagEnd

  lazy val elsifTag: PackratParser[ExpressionAST] = tagStart ~> "elsif" ~> expression <~ tagEnd

  lazy val caseTag: PackratParser[ExpressionAST] = tagStart ~> "case" ~> expression <~ tagEnd

  lazy val whenTag: PackratParser[ExpressionAST] = tagStart ~> "when" ~> expression <~ tagEnd

  lazy val objectOutput: PackratParser[ExpressionOutputStatementAST] = """\{\{-?""".r ~> expression <~ "-?}}".r ^^ ExpressionOutputStatementAST

  lazy val expression: PackratParser[ExpressionAST] =
    orExpression

  lazy val orExpression: PackratParser[ExpressionAST] =
    orExpression ~ ("or" ~> andExpression) ^^ { case l ~ r => OrExpressionAST( l, r ) } |
    andExpression

  lazy val andExpression: PackratParser[ExpressionAST] =
    andExpression ~ ("and" ~> comparisonExpression) ^^ { case l ~ r => AndExpressionAST( l, r ) } |
    comparisonExpression

  lazy val comparisonExpression: PackratParser[ExpressionAST] =
    filterExpression ~ ("contains" ~> filterExpression) ^^ { case l ~ r => ContainsExpressionAST( l, r ) } |
    filterExpression ~ ("==" ~> filterExpression) ^^ { case l ~ r => EqExpressionAST( l, r ) } |
    filterExpression ~ ("!=" ~> filterExpression) ^^ { case l ~ r => NeqExpressionAST( l, r ) } |
    filterExpression ~ ("<" ~> filterExpression) ^^ { case l ~ r => LtExpressionAST( l, r ) } |
    filterExpression ~ ("<=" ~> filterExpression) ^^ { case l ~ r => LteExpressionAST( l, r ) } |
    filterExpression ~ (">" ~> filterExpression) ^^ { case l ~ r => GtExpressionAST( l, r ) } |
    filterExpression ~ (">=" ~> filterExpression) ^^ { case l ~ r => GteExpressionAST( l, r ) } |
    filterExpression

  lazy val filterExpression: PackratParser[ExpressionAST] =
    filterExpression ~ ("|" ~> ident <~ ":") ~ rep1sep(applyExpression, ",") ^^
      { case o ~ f ~ a => FilterExpressionAST( o, f, a ) } |
    filterExpression ~ ("|" ~> ident) ^^ { case o ~ f => FilterExpressionAST( o, f, Nil ) } |
    applyExpression

  lazy val applyExpression: PackratParser[ExpressionAST] =
    applyExpression ~ ("." ~> ident) ^^ { case e ~ n => DotExpressionAST( e, n ) } |
    applyExpression ~ ("[" ~> expression <~ "]") ^^ { case e ~ n => ArrayExpressionAST( e, n ) } |
    rangeExpression

	lazy val rangeExpression: PackratParser[ExpressionAST] =
		("(" ~> primaryExpression <~ "..") ~ (primaryExpression <~ ")") ^^ { case from ~ to => RangeExpressionAST( from, to )} |
		primaryExpression

  lazy val primaryExpression: Parser[ExpressionAST] =
    """('([^']*)')|("([^"]*)")""".r ^^ (s => LiteralExpressionAST( s.substring(1, s.length - 1) )) |
    "true" ^^^ LiteralExpressionAST( true ) |
    "false" ^^^ LiteralExpressionAST( false ) |
    ident ^^ VariableExpressionAST |
    floatRegex ^^ { n => LiteralExpressionAST( BigDecimal(n) ) } |
    integerRegex ^^ { n =>
      val x = BigInt( n )

      if (x.isValidInt)
        LiteralExpressionAST( x.toInt )
      else
        LiteralExpressionAST( x ) }

  def apply[T]( grammar: Parser[T], input: String ) =
    parseAll( grammar, input ) match {
      case Success( result, _ ) => result
      case NoSuccess( msg, r ) =>
				println( s"$msg (${r.pos})\n${r.pos.longString}" )
				sys.exit( 1 )
    }

}