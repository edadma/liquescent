//@
package xyz.hyperreal

import java.io.File
import java.time.temporal.TemporalAccessor


package object liquescent {

  val floatRegex = """-?\d+\.\d+""".r
  val integerRegex = """-?\d+""".r

  case object nil {
    override def toString = ""
  }

  def docroot( name: String, settings: Map[Symbol, Any] ) = new File( settings('docroot).toString, name )

  def round( n: BigDecimal, scale: Int, settings: Map[Symbol, Any] ) =
    n.setScale( scale, settings('roundingMode).asInstanceOf[BigDecimal.RoundingMode.Value] )

  def truthy( a: Any ) = a != nil && a != false

  def falsy( a: Any ) = !truthy( a )

  def float( a: String ) = floatRegex.pattern.matcher( a ).matches

  def integer( a: String ) = integerRegex.pattern.matcher( a ).matches

  def integer( a: Number ) = a.isInstanceOf[java.lang.Integer] || a.isInstanceOf[BigInt]

  def number( a: String ) =
    if (float( a ))
      Some( BigDecimal(a) )
    else if (integer( a )) {
      val x = BigInt( a )

      if (x.isValidInt)
        LiteralExpressionAST( x.toInt )
      else
        LiteralExpressionAST( x )
    } else
      None

  def typeof( a: Any ) =
    a match {
      case _: Seq[_] => ArrayType
      case _: Int | _: BigInt | _: BigDecimal => NumberType
      case _: String => StringType
      case _: Boolean => BooleanType
      case `nil` => NilType
      case _: TemporalAccessor => DateTimeType
    }

  def display( a: Any ): String =
    a match {
      case l: List[_] => l map display mkString
      case m: collection.Map[_, _] => m map { case (k, v) => qdisplay(k) + "=>" + qdisplay(v) } mkString ("{", ",", "}")
      case s => String.valueOf( s )
    }

  def qdisplay( a: Any ): String =
    a match {
      case s: String => '"' + s + '"'
      case _ => display( a )
    }

}