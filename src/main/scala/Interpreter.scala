//@
package xyz.hyperreal.liquescent

import java.io.{ByteArrayOutputStream, PrintStream}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer


class Interpreter( filters: Map[String, Filter], assigns: Map[String, Any], strict: Boolean = true ) {

  val vars = new mutable.HashMap[String, Any] ++ assigns
	var scopes = new ArrayBuffer[mutable.HashMap[String, Any]]

	def setVar( name: String, value: Any ): Unit =
		scopes.view.reverse find (_ contains name) match {
			case None => vars(name) = value
			case Some( scope ) => scope(name) = value
		}

	def getVar( name: String ) =
		scopes.view.reverse find (_ contains name) match {
			case None =>
				vars get name match {
					case None => nil
					case Some( v ) => v
				}
			case Some( scope ) => scope(name)
		}

	def enterScope( locals: List[String] ): Unit = {
		scopes += mutable.HashMap( locals map (_ -> nil): _* )
	}

	def exitScope: Unit = {
		scopes remove scopes.length - 1
	}

  def perform( op: StatementAST, out: PrintStream ): Unit =
    op match {
      case PlainOutputStatementAST( s ) => out.print( s )
      case ExpressionOutputStatementAST( expr ) =>
        out.print(
					eval( expr ) match {
            case l: List[_] => l map display mkString
            case s => display( s )
          }
				)
			case AssignStatementAST( name, expr ) => setVar( name, eval(expr) )
			case BlockStatementAST( block ) => block foreach (perform( _, out ))
			case IfStatementAST( cond, els ) =>
				cond find { case (expr, _) => truthy( eval(expr) ) } match {
					case None =>
						els match {
							case None => 
							case Some( elseStatement ) => perform( elseStatement, out )
						}
					case Some( (_, thenStatement) ) => perform( thenStatement, out )
				}
			case CaseStatementAST( exp, cases, els ) =>
				val value = eval( exp )

				cases find { case (expr, _) => eval( expr ) == value } match {
					case None =>
						els match {
							case None =>
							case Some( elseStatement ) => perform( elseStatement, out )
						}
					case Some( (_, whenStatement) ) => perform( whenStatement, out )
				}
			case UnlessStatementAST( cond, els ) =>
				cond find { case (expr, _) => falsy( eval(expr) ) } match {
					case None =>
						els match {
							case None =>
							case Some( elseStatement ) => perform( elseStatement, out )
						}
					case Some( (_, thenStatement) ) => perform( thenStatement, out )
				}
			case CaptureStatementAST( name, body ) =>
				val bytes = new ByteArrayOutputStream

				perform( body, new PrintStream(bytes) )
				setVar( name, bytes.toString )
			case ForStatementAST( name, expr, parameters, body ) =>
				var list =
					eval( expr ) match {
						case s: Seq[_] => s
						case x => sys.error( s"expected array: $x" )
					}

				parameters foreach {
					case ReversedForParameter => list = list.reverse
					case LimitForParameter( limit ) =>
						eval( limit ) match {
							case n: Number => list = list take n.intValue
							case v => sys.error( s"number was expected: $v" )
						}
					case OffsetForParameter( offset ) =>
						eval( offset ) match {
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
							perform( body, out )
						} catch {
							case _: ContinueException =>
						}
				} catch {
					case _: BreakException =>
				}

				exitScope
			case CycleStatementAST( items ) => out.print( display(eval(items(getVar("#idx").asInstanceOf[Int]%items.length))) )
			case BreakStatementAST => throw new BreakException
			case ContinueStatementAST => throw new ContinueException
    }

//  def assignable( arg: Type, parameter: Type ) =
//    arg == parameter || (parameter == FloatType)

  def applyFilter( operand: Any, filter: Filter, args: List[Any] ) = {
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
            filter.invoke( fargs )
          else
            applyFilter( tail )
      }

    applyFilter( filter.parameters )
  }

  def eval( expr: ExpressionAST ): Any =
    expr match {
			case RangeExpressionAST( from, to ) =>
				((eval( from ), eval( to )) match {
					case (a: Int, b: Int) => a to b
					case (a: BigDecimal, b: BigDecimal) => a to b by 1
					case (a: BigInt, b: BigInt) => a to b by 1
					case (a, b) => sys.error( s"invalid range limits: $a, $b" )
				}) toList
      case DotExpressionAST( expr, name ) =>
        eval( expr ) match {
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

                applyFilter( o, f, Nil )
            }
        }
      case FilterExpressionAST( operand, name, args ) =>
        filters get name match {
          case None => sys.error( s"unknown filter: $name" )
          case Some( f ) => applyFilter( eval(operand), f, args map eval )
        }
      case LiteralExpressionAST( o ) => o
      case VariableExpressionAST( name ) => getVar( name )
			case EqExpressionAST( left, right ) => eval( left ) == eval( right )
			case ContainsExpressionAST( left, right ) =>
				(eval( left ), eval( right )) match {
					case (seq: Seq[_], str: String) => seq contains str
					case (s1: String, s2: String) => s1 contains s2
					case (a, b) => sys.error( s"expected string/array contains string: $a, $b" )
				}
    }

	class BreakException extends RuntimeException

	class ContinueException extends RuntimeException

}