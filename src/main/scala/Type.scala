//@
package xyz.hyperreal.liquescent


trait Type
case object StringType extends Type
case object NumberType extends Type
case object ArrayType extends Type
case object NilType extends Type
case object BooleanType extends Type

case object AnyType extends Type
case object TimestampType extends Type
case object MapType extends Type