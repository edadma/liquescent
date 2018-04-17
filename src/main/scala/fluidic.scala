//@
package xyz.hyperreal


package object fluidic {

  case object nil {
    override def toString = ""
  }

  def typeof( a: Any ) =
    a match {
      case _: Long | _: Double => NumberType
      case _: String => StringType
      case _: Boolean => BooleanType
      case nil => NilType
    }

}