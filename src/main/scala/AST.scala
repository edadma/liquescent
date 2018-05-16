//@
package xyz.hyperreal.liquescent


trait AST

case class SourceAST( elems: List[StatementAST]) extends AST

trait StatementAST extends AST {
  var ls: Boolean
  var rs: Boolean
}

case class TextOutputStatementAST( s: String ) extends StatementAST {
  var ls = false
  var rs = false
}

case class ExpressionOutputStatementAST( expr: ExpressionAST, var ls: Boolean, var rs: Boolean ) extends StatementAST

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
case class FilterExpressionAST( operand: ExpressionAST, name: String, args: List[(String, ExpressionAST)] ) extends ExpressionAST
case class RangeExpressionAST( from: ExpressionAST, to: ExpressionAST ) extends ExpressionAST

case class IfStatementAST( cond: Seq[(ExpressionAST, StatementAST)], els: Option[StatementAST], var ls: Boolean, var rs: Boolean ) extends StatementAST
case class CaseStatementAST( expr: ExpressionAST, cond: Seq[(ExpressionAST, StatementAST)], els: Option[StatementAST], var ls: Boolean, var rs: Boolean ) extends StatementAST
case class UnlessStatementAST( cond: Seq[(ExpressionAST, StatementAST)], els: Option[StatementAST], var ls: Boolean, var rs: Boolean ) extends StatementAST
case class BlockStatementAST( block: Seq[StatementAST], var ls: Boolean, var rs: Boolean ) extends StatementAST
//case class GroupStatementAST( group: Seq[StatementAST] ) extends StatementAST
case class AssignStatementAST( name: String, expr: ExpressionAST, var ls: Boolean, var rs: Boolean ) extends StatementAST
case class CaptureStatementAST( name: String, body: StatementAST, var ls: Boolean, var rs: Boolean ) extends StatementAST
case class IncrementStatementAST( name: String, var ls: Boolean, var rs: Boolean ) extends StatementAST
case class DecrementStatementAST( name: String, var ls: Boolean, var rs: Boolean ) extends StatementAST
case class ForStatementAST( name: String, expr: ExpressionAST, parameters: List[ForParameter], body: StatementAST, var ls: Boolean, var rs: Boolean ) extends StatementAST
case class BreakStatementAST( var ls: Boolean, var rs: Boolean ) extends StatementAST
case class ContinueStatementAST( var ls: Boolean, var rs: Boolean ) extends StatementAST
case class CycleStatementAST( items: Vector[ExpressionAST], var ls: Boolean, var rs: Boolean ) extends StatementAST
case class CustomTagStatementAST( name: String, args: List[ExpressionAST], var ls: Boolean, var rs: Boolean ) extends StatementAST
case class LayoutStatementAST( layout: Option[String], var ls: Boolean, var rs: Boolean ) extends StatementAST
case class IncludeStatementAST( name: String, vars: List[(String, ExpressionAST)], var ls: Boolean, var rs: Boolean ) extends StatementAST
case class RawStatementAST( s: String, var ls: Boolean, var rs: Boolean ) extends StatementAST
case class CommentStatementAST( s: String, var ls: Boolean, var rs: Boolean ) extends StatementAST

trait ForParameter
case object ReversedForParameter extends ForParameter
case class OffsetForParameter( offset: ExpressionAST ) extends ForParameter
case class LimitForParameter( limit: ExpressionAST ) extends ForParameter
