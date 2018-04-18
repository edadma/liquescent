//@
package xyz.hyperreal.fluidic


trait Type
case object StringType extends Type
case object NumberType extends Type
case object ArrayType extends Type
case object NilType extends Type
case object BooleanType extends Type

case object DateTimeType extends Type