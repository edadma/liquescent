//@
package xyz.hyperreal.fluidic

import java.io.PrintStream

import scala.collection.mutable


class Interpreter( filters: Map[String, Filter], out: PrintStream, strict: Boolean = true ) {

  val vars = new mutable.HashMap[String, Any]

  def perform( op: OperationAST ): Unit =
    op match {
      case PlainOutputAST( s ) => out.print( s )
      case ExpressionOutputAST( expr ) => out.print( eval(expr) )
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
          case Some( f ) => applyFilter( eval(operand), f, args )
        }
      case LiteralExpressionAST( o ) => o
      case VariableExpressionAST( name ) =>
        vars get name match {
          case None => nil
          case Some( v ) => v
        }

    }

}