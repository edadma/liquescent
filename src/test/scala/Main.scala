package xyz.hyperreal.fluidic


object Main extends App {

//  val parser = new FluidicParser
  val input =
    """
      asdf {{ "March 14, 2016" | date: "%b %d, %y" }} zxcv
    """

//  println( parser( input ) )

  val interp = new Interpreter( BuiltinFilters.map, Console.out )

  FluidicParser.parse(input) foreach (op => interp.perform(op))
}