//@
package xyz.hyperreal.fluidic


trait AST

case class SourceAST( elems: List[OperationAST]) extends AST

trait OperationAST extends AST
case class PlainOutputAST( s: String ) extends OperationAST
case class ExpressionOutputAST( expr: ExpressionAST ) extends OperationAST

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
case class FilterExpressionAST( operand: ExpressionAST, name: String, args: List[ExpressionAST] ) extends ExpressionAST
