//@
package xyz.hyperreal.liquescent


trait AST

case class SourceAST( elems: List[StatementAST]) extends AST

trait StatementAST extends AST
case class PlainOutputStatementAST( s: String ) extends StatementAST
case class ExpressionOutputStatementAST( expr: ExpressionAST ) extends StatementAST

trait ExpressionAST extends AST
case class DotExpressionAST( expr: ExpressionAST, name: String ) extends ExpressionAST
case class ArrayExpressionAST( expr: ExpressionAST, index: ExpressionAST ) extends ExpressionAST
case class LiteralExpressionAST( o: Any ) extends ExpressionAST
case class VariableExpressionAST( name: String ) extends ExpressionAST
case class OrExpressionAST( left: ExpressionAST, right: ExpressionAST ) extends ExpressionAST
case class AndExpressionAST( left: ExpressionAST, right: ExpressionAST ) extends ExpressionAST
case class EqExpressionAST( left: ExpressionAST, right: ExpressionAST ) extends ExpressionAST
case class NeqExpressionAST( left: ExpressionAST, right: ExpressionAST ) extends ExpressionAST
case class LtExpressionAST( left: ExpressionAST, right: ExpressionAST ) extends ExpressionAST
case class LteExpressionAST( left: ExpressionAST, right: ExpressionAST ) extends ExpressionAST
case class GtExpressionAST( left: ExpressionAST, right: ExpressionAST ) extends ExpressionAST
case class GteExpressionAST( left: ExpressionAST, right: ExpressionAST ) extends ExpressionAST
case class ContainsExpressionAST( left: ExpressionAST, right: ExpressionAST ) extends ExpressionAST
case class FilterExpressionAST( operand: ExpressionAST, name: String, args: List[ExpressionAST] ) extends ExpressionAST
case class RangeExpressionAST( from: ExpressionAST, to: ExpressionAST ) extends ExpressionAST

case class IfStatementAST( cond: Seq[(ExpressionAST, StatementAST)], els: Option[StatementAST] ) extends StatementAST
case class CaseStatementAST( expr: ExpressionAST, cond: Seq[(ExpressionAST, StatementAST)], els: Option[StatementAST] ) extends StatementAST
case class UnlessStatementAST( cond: Seq[(ExpressionAST, StatementAST)], els: Option[StatementAST] ) extends StatementAST
case class BlockStatementAST( block: Seq[StatementAST] ) extends StatementAST
case class AssignStatementAST( name: String, expr: ExpressionAST ) extends StatementAST
case class CaptureStatementAST( name: String, body: StatementAST ) extends StatementAST
case class IncrementStatementAST( name: String ) extends StatementAST
case class DecrementStatementAST( name: String ) extends StatementAST
case class ForStatementAST( name: String, expr: ExpressionAST, parameters: List[ForParameter], body: StatementAST ) extends StatementAST
case object BreakStatementAST extends StatementAST
case object ContinueStatementAST extends StatementAST
case class CycleStatementAST( items: Vector[ExpressionAST] ) extends StatementAST
case class CustomTagStatementAST( name: String, args: List[ExpressionAST] ) extends StatementAST

case class ForGenerator( name: String, expr: ExpressionAST, parameters: List[ForParameter] )

trait ForParameter
case object ReversedForParameter extends ForParameter
case class OffsetForParameter( offset: ExpressionAST ) extends ForParameter
case class LimitForParameter( limit: ExpressionAST ) extends ForParameter
