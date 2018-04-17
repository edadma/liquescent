package xyz.hyperreal.fluidic


object Main extends App {

//  val parser = new FluidicParser
  val input = """asdf {{ 5 | abs }} zxcv"""

//  println( parser( input ) )

  val interp = new Interpreter( BuiltinFilters.map, Console.out )

  FluidicParser.parse(input) foreach (op => interp.perform(op))
}