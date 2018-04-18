//@
package xyz.hyperreal.fluidic

import java.io.PrintStream

import scala.collection.mutable


class Interpreter( filters: Map[String, Filter], assigns: Map[String, Any], out: PrintStream, strict: Boolean = true ) {

  val vars = new mutable.HashMap[String, Any] ++ assigns

  def perform( op: OperationAST ): Unit =
    op match {
      case PlainOutputAST( s ) => out.print( s )
      case ExpressionOutputAST( expr ) =>
        out.print(
          eval( expr ) match {
            case l: List[_] => l.mkString
            case s => s
          }
        )
    }

//  def assignable( arg: Type, parameter: Type ) =
//    arg == parameter || (parameter == FloatType)

  def applyFilter( operand: Any, filter: Filter, args: List[Any] ) = {
    val fargs = operand :: args
    val types = fargs map typeof

    def applyFilter( parameters: List[List[Type]] ): Any =
      parameters match {
        case Nil => sys.error( s"filter ${filter.name} not applicable to [${fargs mkString ", "}]" )
        case head :: tail =>
          if (types == head)
            filter.invoke( fargs )
          else
            applyFilter( tail )
      }

    applyFilter( filter.parameters )
  }

  def eval( expr: ExpressionAST ): Any =
    expr match {
      case DotExpressionAST( expr, name ) =>
        eval( expr ) match {
          case m: Map[String, Any] =>
            m get name match {
              case None => nil
              case Some( v ) => v
            }
          case o =>
            filters get name match {
              case None => sys.error( s"unknown filter: $name" )
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

    }

}