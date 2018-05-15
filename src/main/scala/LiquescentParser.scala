//@
package xyz.hyperreal.liquescent

import java.util.Scanner

import scala.collection.mutable.ListBuffer
import scala.util.parsing.combinator._

object LiquescentParser {
  def parse( template: io.Source ): ParseResult = {
    val parser = new ElementParser
    var layout: Option[String] = Some( "theme" )
    val block = parser( parser.source, template.mkString )

    ParseResult( layout, block )
  }
}

//object LiquescentParser {

//  val objectRegex = """\{\{.*}}"""r
//  val tagRegex = """\{%.*%}"""r
//  val textBeforeElementRegex = """.+?(?=\{\{|\{%)"""r
//  val textRegex = """.*"""r

//  val templateRegex = """\{\{.*?}}|\{%.*?%}"""r
//  val delimiterPattern = """[\s%{}-]+""".r.pattern
//  val tagPattern = """[a-zA-Z]+[a-zA-Z0-9]*""".r.pattern
//
//  def elements( src: String ): List[Element] = {
//    val buf = new ListBuffer[Element]
//    val it = templateRegex.findAllIn( src )
//    var after = 0
//
//    while (it.hasNext) {
//      it.next
//
//      if (it.start != after)
//        buf += TextElement( src.substring(after, it.start) )
//
//      buf +=
//        (src.charAt( it.start + 1 ) match {
//          case '{' => ObjectElement( it.matched )
//          case '%' =>
//            val matched = it.matched
//            val scanner = new Scanner( matched ) useDelimiter delimiterPattern
//
//            TagElement( scanner.next(tagPattern), matched )
//        })
//      after = it.end
//    }
//
//    if (after != src.length)
//      buf += TextElement( src.substring(after, src.length) )
//
//    buf.toList
//  }
//
//  def whitespaceControl( elems: List[Element] ): List[Element] =
//    elems match {
//      case TextElement( pre ) :: (t@TagElement( _, tag )) :: tail if tag startsWith "{%-" =>
//        if (pre forall (_.isWhitespace))
//          whitespaceControl( t :: tail )
//        else
//          TextElement(pre.reverse dropWhile (_.isWhitespace) reverse) :: whitespaceControl( t :: tail )
//      case (t@TagElement( _, tag )) :: TextElement( post ) :: tail if tag endsWith "-%}" =>
//        if (post forall (_.isWhitespace))
//          whitespaceControl( t :: tail )
//        else
//          t :: whitespaceControl( TextElement( post dropWhile (_.isWhitespace) ) :: tail )
//      case TextElement( pre ) :: (o@ObjectElement( obj )) :: tail if obj startsWith "{{-" =>
//        if (pre forall (_.isWhitespace))
//          whitespaceControl( o :: tail )
//        else
//          TextElement(pre.reverse dropWhile (_.isWhitespace) reverse) :: whitespaceControl( o :: tail )
//      case (o@ObjectElement( obj )) :: TextElement( post ) :: tail if obj endsWith "-}}" =>
//        if (post forall (_.isWhitespace))
//          whitespaceControl( o :: tail )
//        else
//          o :: whitespaceControl( TextElement( post dropWhile (_.isWhitespace) ) :: tail )
//      case head :: tail => head :: whitespaceControl( tail )
//      case Nil => Nil
//    }

//  def parse( template: io.Source ): ParseResult = {
//    var tokens = whitespaceControl( elements(template mkString) filterNot new CommentFilter flatMap new RawTransform )
//    var layout: Option[String] = Some( "theme" )
//
//    def peek = tokens.head
//
//    def token( tok: String ) =
//      peek match {
//        case TagElement( `tok`, _ ) => true
//        case _ => false
//      }
//
//    def tokenAdvance( tok: String ) =
//      if (token( tok )) {
//        advance
//        true
//      } else
//        false
//
//    def pop = {
//      val t = peek
//
//      advance
//      t
//    }
//
//    def popTag = pop.asInstanceOf[TagElement]
//
//    def advance = tokens = tokens.tail
//
//    def consume( tok: String ) =
//      if (eoi) {
//        sys.error( s" expected '$tok' tag, but end of input encountered" )
//      } else if (!tokenAdvance( tok )) {
//        sys.error( s" expected '$tok' tag, but '$peek' encountered" )
//      }
//
//    def eoi = tokens == Nil
//
//    def parseIf( s: String ) = {
//      val parser = new ElementParser
//      val cond = parser( parser.ifTag, s )
//      val conds = new ListBuffer[(ExpressionAST, StatementAST)]
//
//      conds += cond -> parseBlock
//
//      while (token( "elsif" )) {
//        val parser = new ElementParser
//        val cond = parser( parser.elsifTag, popTag.s )
//
//        conds += cond -> parseBlock
//      }
//
//      val els =
//        if (tokenAdvance( "else" ))
//          Some( parseBlock )
//        else
//          None
//
//      consume( "endif" )
//      IfStatementAST( conds.toList, els )
//    }
//
//    def parseUnless( s: String ) = {
//      val parser = new ElementParser
//      val cond = parser( parser.unlessTag, s )
//      val conds = new ListBuffer[(ExpressionAST, StatementAST)]
//
//      conds += cond -> parseBlock
//
//      while (token( "elsif" )) {
//        val parser = new ElementParser
//        val cond = parser( parser.elsifTag, popTag.s )
//
//        conds += cond -> parseBlock
//      }
//
//      val els =
//        if (tokenAdvance( "else" ))
//          Some( parseBlock )
//        else
//          None
//
//      consume( "endunless" )
//      UnlessStatementAST( conds.toList, els )
//    }
//
//    def parseCase( s: String ) = {
//      val parser = new ElementParser
//      val expr = parser( parser.caseTag, s )
//      val cases = new ListBuffer[(ExpressionAST, StatementAST)]
//
//      parseBlock
//
//      while (token( "when" )) {
//        val parser = new ElementParser
//        val when = parser( parser.whenTag, popTag.s )
//
//        cases += when -> parseBlock
//      }
//
//      val els =
//        if (tokenAdvance( "else" ))
//          Some( parseBlock )
//        else
//          None
//
//      consume( "endcase" )
//      CaseStatementAST( expr, cases.toList, els )
//    }
//
//    def parseFor( s: String ) = {
//      val parser = new ElementParser
//      val ForGenerator( name, expr, parameters ) = parser( parser.forTag, s )
//			val body = parseBlock
//
//      consume( "endfor" )
//      ForStatementAST( name, expr, parameters, body )
//    }
//
//    def parseBlock: StatementAST = {
//      val block = new ListBuffer[StatementAST]
//
//      def _parseBlock: Unit =
//        if (!eoi) {
//          peek match {
//            case TextElement( s ) =>
//              advance
//              block += PlainOutputStatementAST( s )
//              _parseBlock
//            case ObjectElement( s ) =>
//              val parser = new ElementParser
//
//              advance
//              block += parser( parser.objectOutput, s )
//              _parseBlock
//            case TagElement( "break", _ ) =>
//              advance
//              block += BreakStatementAST
//              _parseBlock
//            case TagElement( "continue", _ ) =>
//              advance
//              block += ContinueStatementAST
//              _parseBlock
//            case TagElement( "cycle", s ) =>
//              val parser = new ElementParser
//
//              advance
//              block += parser( parser.cycleTag, s )
//              _parseBlock
//            case TagElement( "layout", s ) =>
//              val parser = new ElementParser
//
//              parser( parser.layoutTag, s ) match {
//                case LayoutStatementAST( l ) => layout = l
//              }
//
//              advance
//              _parseBlock
//             case TagElement( "include", s ) =>
//              val parser = new ElementParser
//
//              advance
//              block += parser( parser.includeTag, s )
//              _parseBlock
//           case TagElement( "increment", s ) =>
//              val parser = new ElementParser
//
//              advance
//              block += parser( parser.incrementTag, s )
//              _parseBlock
//             case TagElement( "decrement", s ) =>
//              val parser = new ElementParser
//
//              advance
//              block += parser( parser.decrementTag, s )
//              _parseBlock
//           case TagElement( "if", s ) =>
//              advance
//              block += parseIf( s )
//              _parseBlock
//            case TagElement( "unless", s ) =>
//              advance
//              block += parseUnless( s )
//              _parseBlock
//            case TagElement( "case", s ) =>
//              advance
//              block += parseCase( s )
//              _parseBlock
//            case TagElement( "for", s ) =>
//              advance
//              block += parseFor( s )
//              _parseBlock
//            case TagElement( "assign", s ) =>
//              val parser = new ElementParser
//
//              advance
//              block += parser( parser.assignTag, s )
//              _parseBlock
//            case TagElement( "capture", s ) =>
//              val parser = new ElementParser
//
//              advance
//              block += CaptureStatementAST( parser(parser.captureTag, s), parseBlock )
//              consume( "endcapture" )
//              _parseBlock
//            case TagElement( "endif"|"endfor"|"endcase"|"endunless"|"endtablerow"|"endcapture"|"else"|"elsif"|"when", _ ) =>
//            case TagElement( _, s ) =>
//              val parser = new ElementParser
//
//              advance
//              block += parser( parser.customTag, s )
//              _parseBlock
//          }
//        }
//
//      _parseBlock
//
//      if (block.length == 1)
//        block.head
//      else
//        BlockStatementAST( block toList )
//    }
//
//    val block = parseBlock
//
//    if (!eoi)
//      sys.error( s"unexpected element $pop" )
//
//    ParseResult( layout, block )
//  }
//
//}
//
//class CommentFilter extends (Element => Boolean) {
//  var dropping = false
//
//  def apply( elem: Element ) =
//    elem match {
//      case TagElement( "comment", _ ) if !dropping =>
//        dropping = true
//        true
//      case TagElement( "endcomment", _ ) if dropping =>
//        dropping = false
//        true
//      case _ => dropping
//    }
//}
//
//class RawTransform extends (Element => Seq[Element]) {
//  var raw = false
//
//  def apply( elem: Element ) =
//    elem match {
//      case TagElement( "raw", _ ) if !raw =>
//        raw = true
//        Nil
//      case TagElement( "endraw", _ ) if raw =>
//        raw = false
//        Nil
//      case TagElement( _, s ) if raw => List( TextElement(s) )
//      case ObjectElement( s ) if raw => List( TextElement(s) )
//      case _ => List( elem )
//    }
//}
//
//class LayoutFilter extends (Element => Boolean) {
//  var dropping = false
//
//  def apply( elem: Element ) =
//    elem match {
//      case TextElement( s ) if !dropping && s.forall( _.isWhitespace ) =>
//        false
//      case TagElement( "layout", _ ) if dropping =>
//        false
//      case _ =>
//        dropping = true
//        true
//    }
//}
//
//trait Element
//
//case class TextElement( s: String ) extends Element
//case class ObjectElement( s: String ) extends Element
//case class TagElement( tag: String, s: String ) extends Element

/*
  def text: Parser[Element] =
    """(?s).*?(?=\{\{|\{%)|.+""".r ^^ (s => Element('text, s) )

  def obj: Parser[Element] = """\{\{-?""".r ~ ".+?(?=}})".r ~ """-?}}\s*""".r ^^ {
    case start ~ s ~ end => Element('object, s"$start|$s|$end")
  }

  def tag: Parser[Element] = "{%" ~> ".+?(?=%})".r <~ "%}" ^^ (s => Element('tag, s) )

  def source: Parser[List[Element]] = rep( element )

  def element: Parser[Element] = obj | tag | text
 */
class ElementParser extends RegexParsers with PackratParsers {

  override val skipWhitespace = false

  override implicit def literal(s: String): Parser[String] = (in: Input) => {
      val source = in.source
      val offset = in.offset
      val start = handleWhiteSpace(source, offset)
      var i = 0
      var j = start

      while (i < s.length && j < source.length && s.charAt(i) == source.charAt(j)) {
        i += 1
        j += 1
      }

      if (i == s.length) {
        while (j < source.length && source.charAt(j).isWhitespace)
          j += 1

        Success(source.subSequence(start, j).toString, in.drop(j - offset))
      } else  {
        val found = if (start == source.length()) "end of source" else "`"+source.charAt(start)+"'"
        Failure("`"+s+"' expected but "+found+" found", in.drop(start - offset))
      }
    }

  lazy val source: PackratParser[StatementAST] = block ~ opt(endTextOutput) ^^ {
    case b ~ None => b
    case BlockStatementAST( b, _, _ ) ~ Some( t ) => BlockStatementAST( b :+ t, false, false )
    case s ~ Some( t ) => BlockStatementAST( List(s, t), false, false )
  }

  lazy val block: PackratParser[StatementAST] = rep(statement) ^^ (l => if (l.length == 1) l.head else BlockStatementAST( l, l.head.ls, l.last.rs ))

  lazy val statement: PackratParser[StatementAST] = tag | objectOutput | textOutput

  lazy val textOutput: PackratParser[TextOutputStatementAST] = guard(not("{{" | "{%")) ~> """(?s).+?(?=\{\{|\{%)""".r ^^ TextOutputStatementAST

  lazy val endTextOutput: PackratParser[TextOutputStatementAST] = """(?s).+""".r ^^ TextOutputStatementAST

  lazy val ident: PackratParser[String] = """[a-zA-Z_]+\w*\s*""".r ^^ (_.trim)

  lazy val tag: PackratParser[StatementAST] =
    assignTag |
    ifTag |
    unlessTag |
    cycleTag |
    incrementTag |
    decrementTag |
    captureTag |
    caseTag

  lazy val tagStart = """\{%-?\s*"""r

  lazy val tagEnd = "-?%}"r

  lazy val assignTag: PackratParser[StatementAST] = (tagStart <~ "assign") ~ (ident <~ "=") ~ expression ~ tagEnd ^^ {
    case ts ~ n ~ e ~ te => AssignStatementAST( n, e, ts contains '-', te contains '-' ) }

  lazy val captureTag: PackratParser[StatementAST] =
    (tagStart <~ "capture") ~ ident ~ tagEnd ~ block ~ (tagStart <~ "endcapture") ~ tagEnd ^^ {
      case its ~ v ~ ite ~ b ~ ets ~ ete => CaptureStatementAST( v, b, its contains '-', ete contains '-' )
    }

  lazy val ifTag: PackratParser[StatementAST] =
    (tagStart <~ "if") ~ expression ~ tagEnd ~ block ~ rep(elsif) ~ opt(elsePart) ~ (tagStart <~ "endif") ~ tagEnd ^^ {
      case its ~ e ~ ite ~ b ~ eis ~ els ~ ets ~ ete => IfStatementAST( (e, b) +: eis, els, its contains '-', ete contains '-' )
    }

	lazy val elsif: PackratParser[(ExpressionAST, StatementAST)] = ((tagStart <~ "elsif") ~> expression <~ tagEnd) ~ block ^^ {
    case e ~ b => (e, b)
  }

	lazy val elsePart: PackratParser[StatementAST] = (tagStart <~ "else") ~> tagEnd ~> block

  lazy val unlessTag: PackratParser[StatementAST] =
    (tagStart <~ "unless") ~ expression ~ tagEnd ~ block ~ rep(elsif) ~ opt(elsePart) ~ (tagStart <~ "endunless") ~ tagEnd ^^ {
      case its ~ e ~ ite ~ b ~ eis ~ els ~ ets ~ ete => UnlessStatementAST( (e, b) +: eis, els, its contains '-', ete contains '-' )
    }

  lazy val caseTag: PackratParser[StatementAST] =
    (tagStart <~ "case") ~ expression ~ tagEnd ~ block ~ rep(when) ~ opt(elsePart) ~ (tagStart <~ "endcase") ~ tagEnd ^^ {
      case its ~ e ~ ite ~ b ~ eis ~ els ~ ets ~ ete => CaseStatementAST( e, eis, els, its contains '-', ete contains '-' )
    }

	lazy val when: PackratParser[(ExpressionAST, StatementAST)] = ((tagStart <~ "when") ~> expression <~ tagEnd) ~ block ^^ {
    case e ~ b => (e, b)
  }

  lazy val cycleTag: PackratParser[StatementAST] = tagStart ~> "cycle" ~> rep1sep(expression, ",") <~ tagEnd ^^ { xs => CycleStatementAST( xs.toVector, false, false ) }

  lazy val forTag: PackratParser[ForStatementAST] =
    (tagStart ~> "for" ~> ((ident <~ "in") ~ expression)) ~ (rep(forParameters) <~ tagEnd) ~ block ~ (tagStart <~ "endcase") ~ tagEnd ^^ {
      case n ~ e ~ p ~ b => ForStatementAST( n, e, p, b, false, false ) }

  lazy val forParameters: PackratParser[ForParameter] =
    "reversed" ^^^ ReversedForParameter |
    "offset" ~> ":" ~> expression ^^ OffsetForParameter |
    "limit" ~> ":" ~> expression ^^ LimitForParameter

  lazy val customTag: PackratParser[CustomTagStatementAST] = tagStart ~> (ident ~ repsep(expression, ",")) <~ tagEnd ^^ {
    case n ~ a => CustomTagStatementAST( n, a, false, false ) }

  lazy val incrementTag: PackratParser[IncrementStatementAST] = tagStart ~> "increment" ~> ident <~ tagEnd ^^ (v => IncrementStatementAST( v, false, false ))

  lazy val includeTag: PackratParser[IncludeStatementAST] =
    (tagStart ~> "include" ~> string) ~ ("with" ~> expression <~ tagEnd) ^^ {
      case f ~ v => IncludeStatementAST( f, List((f, v)), false, false ) } |
    (tagStart ~> "include" ~> string) ~ (opt("," ~> rep1sep(includeArgument, ",")) <~ tagEnd) ^^ {
      case f ~ None => IncludeStatementAST( f, Nil, false, false )
      case f ~ Some( a ) => IncludeStatementAST( f, a, false, false )
    }

  lazy val includeArgument: PackratParser[(String, ExpressionAST)] =
    (ident <~ ":") ~ expression ^^ {
      case k ~ v => (k, v)
    }

//  lazy val layoutTag: PackratParser[LayoutStatementAST] =
//    tagStart ~> "layout" ~> string <~ tagEnd ^^ (s => LayoutStatementAST( Some(s) )) |
//    tagStart ~> "layout" ~> "none" <~ tagEnd ^^^ LayoutStatementAST( None )

  lazy val decrementTag: PackratParser[DecrementStatementAST] = tagStart ~> "decrement" ~> ident <~ tagEnd ^^ (v => DecrementStatementAST( v, false, false ))

  lazy val objectOutput: PackratParser[ExpressionOutputStatementAST] =
    """\{\{-?\s*""".r ~ expression ~ "-?}}".r ^^ {
      case ts ~ e ~ te => ExpressionOutputStatementAST( e, ts contains '-', te contains '-' )
    }

  lazy val ws = "\\s*".r

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

  lazy val filterArgument: PackratParser[(String, ExpressionAST)] =
    opt(ident <~ ":") ~ applyExpression ^^ {
      case Some( k ) ~ v => (k, v)
      case None ~ v => (null, v)
    }

  lazy val filterExpression: PackratParser[ExpressionAST] =
    filterExpression ~ ("|" ~> ident <~ ":") ~ rep1sep(filterArgument, ",") ^^
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

  lazy val string: Parser[String] =
    """(('([^']*)')|("([^"]*)"))\s*""".r ^^ {s =>
      val trimed = s.trim

      escapes(trimed.substring( 1, trimed.length - 1))
    }

  lazy val primaryExpression: Parser[ExpressionAST] =
    string ^^ (s => LiteralExpressionAST( s )) |
    "true" ^^^ LiteralExpressionAST( true ) |
    "false" ^^^ LiteralExpressionAST( false ) |
    ident ^^ VariableExpressionAST |
    (floatRegex <~ ws) ^^ { n => LiteralExpressionAST( BigDecimal(n) ) } |
    (integerRegex <~ ws) ^^ { n =>
      val x = BigInt( n )

      if (x.isValidInt)
        LiteralExpressionAST( x.toInt )
      else
        LiteralExpressionAST( x ) }

  val unicodeRegex = "\\\\u[0-9a-fA-F]{4}".r

  def escapes( s: String ) =
    unicodeRegex.replaceAllIn(
      s
      .replace( """\b""", "\b" )
      .replace( """\t""", "\t" )
      .replace( """\f""", "\f" )
      .replace( """\n""", "\n" )
      .replace( """\r""", "\r" )
      .replace( """\\""", "\\" )
      .replace( """\"""", "\"" )
      .replace( """\'""", "\'" ),
      m => Integer.parseInt( m.matched.substring(2), 16 ).toChar.toString
    )

  def apply[T]( grammar: Parser[T], input: String ) =
    parseAll( grammar, input ) match {
      case Success( result, _ ) => result
      case NoSuccess( msg, r ) => sys.error( s"$msg (${r.pos})\n${r.pos.longString}" )
    }

}

case class ParseResult( layout: Option[String], statement: StatementAST )