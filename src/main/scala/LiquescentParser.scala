//@
package xyz.hyperreal.liquescent

import scala.util.parsing.combinator._


object LiquescentParser {
  def parse( template: io.Source ): ParseResult = {
    val parser = new LiquescentParser
    var layout: Option[String] = Some( "theme" )
    val block = parser( parser.source, template.mkString )

    ParseResult( layout, block )
  }
}

class LiquescentParser extends RegexParsers with PackratParsers {

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
    case BlockStatementAST( Nil, _, _ ) ~ Some( t ) => t
    case BlockStatementAST( b, _, _ ) ~ Some( t ) => BlockStatementAST( b :+ t, false, false )
    case s ~ Some( t ) => BlockStatementAST( List(s, t), false, false )
  }

  lazy val block: PackratParser[StatementAST] = rep(statement) ^^ {
    case Nil => BlockStatementAST( Nil, false, false )
    case List( s ) => s
    case l => BlockStatementAST( l, l.head.ls, l.last.rs )
  }

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
    caseTag |
    forTag |
    breakTag |
    continueTag |
    commentTag |
    rawTag |
    includeTag |
    layoutTag |
    customTag

  lazy val tagStart = """\{%-?\s*"""r

  lazy val tagEnd = "-?%}"r

  lazy val rawTag: PackratParser[StatementAST] =
    (tagStart <~ "raw") ~ tagEnd ~ """(?s).*?(?=\{%\s*endraw\s*%})""".r ~ (tagStart <~ "endraw") ~ tagEnd ^^ {
      case rts ~ rte ~ t ~ ets ~ ete => RawStatementAST( t, rts contains '-', ete contains '-' )
    }

  lazy val commentTag: PackratParser[StatementAST] =
    (tagStart <~ "comment") ~ tagEnd ~ """(?s).*?(?=\{%\s*endcomment\s*%})""".r ~ (tagStart <~ "endcomment") ~ tagEnd ^^ {
      case rts ~ rte ~ t ~ ets ~ ete => CommentStatementAST( t, rts contains '-', ete contains '-' )
    }

  lazy val breakTag: PackratParser[StatementAST] = (tagStart <~ "break") ~ tagEnd ^^ {
    case ts ~ te => BreakStatementAST( ts contains '-', te contains '-' )
  }

  lazy val continueTag: PackratParser[StatementAST] = (tagStart <~ "continue") ~ tagEnd ^^ {
    case ts ~ te => ContinueStatementAST( ts contains '-', te contains '-' )
  }

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
    (tagStart <~ "for") ~ (ident <~ "in") ~ expression ~ rep(forParameters) ~ tagEnd ~ block ~ (tagStart <~ "endfor") ~ tagEnd ^^ {
      case fts ~ n ~ e ~ p ~ fte ~ b ~ ets ~ ete => ForStatementAST( n, e, p, b, false, false )
    }

  lazy val forParameters: PackratParser[ForParameter] =
    "reversed" ^^^ ReversedForParameter |
    "offset" ~> ":" ~> expression ^^ OffsetForParameter |
    "limit" ~> ":" ~> expression ^^ LimitForParameter

  lazy val customTag: PackratParser[CustomTagStatementAST] =
    tagStart ~> ((guard(not(
      "for"|"endfor"|"if"|"endif"|"else"|"elsif"|"case"|"when"|"endcase"|"unless"|"endunless"|"capture"|"endcapture"|"raw"|"endraw"|"comment"|"endcomment"|"include"|"layout"
      )) ~> ident) ~ repsep(expression, ",")) <~ tagEnd ^^ {
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

  lazy val layoutTag: PackratParser[LayoutStatementAST] =
    tagStart ~ ("layout" ~> "none" ~> tagEnd) ^^ {
      case ts ~ te => LayoutStatementAST( None, ts contains '-', te contains '-' ) } |
    tagStart ~ ("layout" ~> string) ~ tagEnd ^^ {
      case ts ~ s ~ te => LayoutStatementAST( Some(s), ts contains '-', te contains '-' ) }

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
