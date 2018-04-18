//@
package xyz.hyperreal


package object fluidic {

  case object nil {
    override def toString = ""
  }

  def typeof( a: Any ) =
    a match {
      case _: Seq[_] => ArrayType
      case _: Long | _: Double => NumberType
      case _: String => StringType
      case _: Boolean => BooleanType
      case `nil` => NilType
    }

  def display( a: Any ): String =
    a match {
      case l: List[_] => l map display mkString
      case m: collection.Map[_, _] => m map { case (k, v) => qdisplay(k) + "=>" + qdisplay(v) } mkString ("{", ",", "}")
      case s => s.toString
    }

  def qdisplay( a: Any ): String =
    a match {
      case s: String => '"' + s + '"'
      case _ => display( a )
    }

}