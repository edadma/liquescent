package xyz.hyperreal.fluidic


object Main extends App {

//  val parser = new FluidicParser
  val input =
    """
      asdf {{ "2007-12-03T10:15:30" | date: "%Y-%m-%d %H:%M" }} zxcv
    """

//  println( parser( input ) )

  val interp = new Interpreter( BuiltinFilters.map, Console.out )

  FluidicParser.parse(input) foreach (op => interp.perform(op))
}