//@
package xyz.hyperreal.liquescent

import java.io.{ByteArrayOutputStream, File, PrintStream}

import xyz.hyperreal.lia.Math
import xyz.hyperreal.numbers.BigDecimalMath

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer


class Interpreter( filters: Map[String, Filter], tags: Map[String, Tag], settings: Map[Symbol, Any], assigns: Map[String, Any], context: AnyRef ) {

  val vars = new mutable.HashMap[String, Any] ++ assigns
	val scopes = new ArrayBuffer[mutable.HashMap[String, Any]]
	val incdec = new mutable.HashMap[String, BigInt]

  Math.bdmath = new BigDecimalMath( 16 )

	def setVar( name: String, value: Any ): Unit =
		scopes.view.reverse find (_ contains name) match {
			case None => vars(name) = value
			case Some( scope ) => scope(name) = value
		}

	def getVar( name: String, locals: Map[String, String] ) =
		scopes.view.reverse find (_ contains name) match {
			case None =>
        locals get name match {
          case None =>
            vars get name match {
              case None => nil
              case Some( v ) => v
            }
          case Some( v ) => v
        }
			case Some( scope ) => scope(name)
		}

  def capture( statement: StatementAST, locals: Map[String, String] ) = {
    val bytes = new ByteArrayOutputStream

    execute( statement, locals, new PrintStream(bytes) )
    bytes.toString
  }

	def enterScope( locals: List[String] ): Unit = scopes += mutable.HashMap( locals map (_ -> nil): _* )

	def exitScope: Unit = scopes remove scopes.length - 1

  def render( parse: ParseResult, locals: Map[String, String], out: PrintStream, dolayout: Boolean ): Unit = {
    if (dolayout && parse.layout.nonEmpty) {
      setVar( "content_for_layout", capture(parse.statement, locals) )

      val file = new File( docroot("layout", settings), parse.layout.get + ".liquid" )

      if (parse.layout.get == "theme" && file.exists && file.isFile && file.canRead || parse.layout.get != "theme")
        include( file, locals, out )
      else
        execute( parse.statement, locals, out )
    } else
      execute( parse.statement, locals, out )
  }

  def include( input: File, locals: Map[String, String], out: PrintStream ) =
    execute( LiquescentParser.parse(io.Source.fromFile(input)).statement, locals, out )

  def execute( op: StatementAST, locals: Map[String, String], out: PrintStream ): Unit = {
    op match {
      case LayoutStatementAST( _ ) =>
      case PlainOutputStatementAST( s ) => out.print( s )
      case ExpressionOutputStatementAST( expr ) =>
        out.print(
					eval( expr, locals ) match {
            case l: List[_] => l map display mkString
            case s => display( s )
          }
				)
			case AssignStatementAST( name, expr ) => setVar( name, eval(expr, locals) )
			case IncrementStatementAST( name ) =>
				out.print( incdec get name match {
					case None =>
						incdec(name) = 0
						0
					case Some( v ) =>
						val res = v + 1

						incdec(name) = res
						res
				} )
			case DecrementStatementAST( name ) =>
				out.print( incdec get name match {
					case None =>
						incdec(name) = -1
						-1
					case Some( v ) =>
						val res = v - 1

						incdec(name) = res
						res
				} )
			case CustomTagStatementAST( name, args ) =>
				tags get name match {
					case None => sys.error( s"unknown tag: $name" )
					case Some( t ) => t( settings, vars, out, args map (a => eval(a, locals)), context )
				}
			case BlockStatementAST( block ) => block foreach (execute( _, locals, out ))
			case IfStatementAST( cond, els ) =>
				cond find { case (expr, _) => truthy( eval(expr, locals) ) } match {
					case None =>
						els match {
							case None =>
							case Some( elseStatement ) => execute( elseStatement, locals, out )
						}
					case Some( (_, thenStatement) ) => execute( thenStatement, locals, out )
				}
			case CaseStatementAST( exp, cases, els ) =>
				val value = eval( exp, locals )

				cases find { case (expr, _) => eval( expr, locals ) == value } match {
					case None =>
						els match {
							case None =>
							case Some( elseStatement ) => execute( elseStatement, locals, out )
						}
					case Some( (_, whenStatement) ) => execute( whenStatement, locals, out )
				}
			case UnlessStatementAST( cond, els ) =>
				cond find { case (expr, _) => falsy( eval(expr, locals) ) } match {
					case None =>
						els match {
							case None =>
							case Some( elseStatement ) => execute( elseStatement, locals, out )
						}
					case Some( (_, thenStatement) ) => execute( thenStatement, locals, out )
				}
			case CaptureStatementAST( name, body ) => setVar( name, capture(body, locals) )
			case ForStatementAST( name, expr, parameters, body ) =>
				var list =
					eval( expr, locals ) match {
						case s: Seq[_] => s
						case x => sys.error( s"expected array: $x" )
					}

				parameters foreach {
					case ReversedForParameter => list = list.reverse
					case LimitForParameter( limit ) =>
						eval( limit, locals ) match {
							case n: Number => list = list take n.intValue
							case v => sys.error( s"number was expected: $v" )
						}
					case OffsetForParameter( offset ) =>
						eval( offset, locals ) match {
							case n: Number => list = list drop n.intValue
							case v => sys.error( s"number was expected: $v" )
						}
				}

				enterScope( List(name, "#idx") )

				try {
					for ((elem, idx) <- list zipWithIndex)
						try {
							setVar( name, elem )
							setVar( "#idx", idx )
							execute( body, locals, out )
						} catch {
							case _: ContinueException =>
						}
				} catch {
					case _: BreakException =>
				}

				exitScope
			case CycleStatementAST( items ) => out.print( display(eval(items(getVar("#idx", locals).asInstanceOf[Int]%items.length), locals)) )
			case BreakStatementAST => throw new BreakException
			case ContinueStatementAST => throw new ContinueException
    }
  }

//  def assignable( arg: Type, parameter: Type ) =
//    arg == parameter || (parameter == FloatType)

  def applyFilter( operand: Any, filter: Filter, args: List[Any], named: Map[String, Any] ) = {
    val fargs = operand :: args
    val types = fargs map typeof

    def assignable( args: List[Type], parms: List[Type] ): Boolean =
      (args, parms) match {
        case (Nil, Nil) => true
        case (Nil, List(_)) => false
        case (List(_), Nil) => false
        case (ah :: at, ph :: pt) =>
          if (ah == ph || ph == AnyType)
            assignable( at, pt )
          else
            false
      }

    def applyFilter( parameters: List[List[Type]] ): Any =
      parameters match {
        case Nil => sys.error( s"filter ${filter.name} not applicable to [${fargs mkString ", "}]" )
        case head :: tail =>
          if (assignable( types, head ))
            filter( this, settings, fargs, named )
          else
            applyFilter( tail )
      }

    applyFilter( filter.parameters )
  }

	def compare( a: Any, b: Any ) =
		(a, b) match {
			case (x: Number, y: Number) => Math( '+, Math('-, x, y), BigDecimal(0) ).asInstanceOf[BigDecimal].signum
			case (x: String, y: String) => x compareTo y
			case _ => sys.error( s"expected two numbers or two strings: $a, $b" )
		}

  def eval( expr: ExpressionAST, locals: Map[String, String] ): Any =
    expr match {
			case RangeExpressionAST( from, to ) =>
				((eval( from, locals ), eval( to, locals )) match {
					case (a: Int, b: Int) => a to b
					case (a: BigDecimal, b: BigDecimal) => a to b by 1
					case (a: BigInt, b: BigInt) => a to b by 1
					case (a, b) => sys.error( s"invalid range limits: $a, $b" )
				}) toList
      case ArrayExpressionAST( exp, index ) =>
        (eval( exp, locals ), eval( index, locals )) match {
          case (m: collection.Map[_, _], i: String) => m.asInstanceOf[collection.Map[String, Any]](i)
          case (s: Seq[_], i: Number) => s.asInstanceOf[Seq[Any]]( i.intValue )
          case (a, b) => sys.error( s"bracket syntax not applicable: $a, $b" )
        }
      case DotExpressionAST( exp, name ) =>
        eval( exp, locals ) match {
          case `nil` => nil
          case m: collection.Map[_, _] =>
            m.asInstanceOf[collection.Map[String, Any]] get name match {
              case None => nil
              case Some( v ) => v
            }
          case o =>
            filters get name match {
              case None => sys.error( s"non-existant property: $name" )
              case Some( f ) =>
                if (!f.dottable)
                  sys.error( s"filter not dottable: $name" )

                applyFilter( o, f, Nil, Map() )
            }
        }
      case FilterExpressionAST( operand, name, args ) =>
        filters get name match {
          case None => sys.error( s"unknown filter: $name" )
          case Some( f ) =>
            applyFilter( eval(operand, locals), f,
              args filter (_._1 == null) map {case (_, v) => eval(v, locals)},
              args filterNot (_._1 == null) map {case (k, v) => (k, eval(v, locals))} toMap)
        }
      case LiteralExpressionAST( o ) => o
      case VariableExpressionAST( name ) => getVar( name, locals )
			case OrExpressionAST( left, right ) => truthy( eval(left, locals) ) || truthy( eval(right, locals) )
			case AndExpressionAST( left, right ) => truthy( eval(left, locals) ) && truthy( eval(right, locals) )
			case EqExpressionAST( left, right ) => eval( left, locals ) == eval( right, locals )
			case NeqExpressionAST( left, right ) => eval( left, locals ) != eval( right, locals )
			case LtExpressionAST( left, right ) => compare( eval(left, locals), eval(right, locals) ) < 0
			case LteExpressionAST( left, right ) => compare( eval(left, locals), eval(right, locals) ) <= 0
			case GtExpressionAST( left, right ) => compare( eval(left, locals), eval(right, locals) ) > 0
			case GteExpressionAST( left, right ) => compare( eval(left, locals), eval(right, locals) ) >= 0
			case ContainsExpressionAST( left, right ) =>
				(eval( left, locals ), eval( right, locals )) match {
					case (seq: Seq[_], str: String) => seq contains str
					case (s1: String, s2: String) => s1 contains s2
					case (a, b) => sys.error( s"expected string/array contains string: $a, $b" )
				}
    }

	class BreakException extends RuntimeException

	class ContinueException extends RuntimeException

}