//@
package xyz.hyperreal.liquescent

import java.io.{ByteArrayOutputStream, PrintStream}

import scala.collection.mutable


class Interpreter( filters: Map[String, Filter], assigns: Map[String, Any], strict: Boolean = true ) {

  val vars = new mutable.HashMap[String, Any] ++ assigns

  def perform( op: StatementAST, out: PrintStream ): Unit =
    op match {
      case PlainOutputStatementAST( s ) => out.print( s )
      case ExpressionOutputStatementAST( expr ) =>
        out.print(
					eval( expr ) match {
            case l: List[_] => l.mkString
            case s => s
          }
				)
			case AssignStatementAST( name, expr ) => vars(name) = eval( expr )
			case BlockStatementAST( block ) => block foreach (perform( _, out ))
			case IfStatementAST( cond, els ) =>
				cond find {case (expr, _) => truthy( eval(expr) )} match {
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
				vars(name) = bytes.toString
			case ForStatementAST( name, expr, body ) =>
				val list = eval( expr ).asInstanceOf[Seq[Any]]

				for (elem <- list) {
					vars(name) = elem
					perform( body, out )
				}
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
      case VariableExpressionAST( name ) =>
        vars get name match {
          case None => nil
          case Some( v ) => v
        }
			case EqExpressionAST( left, right ) => eval( left ) == eval( right )
    }

}