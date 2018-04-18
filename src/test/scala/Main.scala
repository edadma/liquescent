package xyz.hyperreal.fluidic


object Main extends App {

//  val parser = new FluidicParser
  val input =
    """
      {{ list | uniq | join: ", " }}
    """

//  println( parser( input ) )

  val interp = new Interpreter( BuiltinFilters.map, Map("list" -> List(1, 3, 2, 3, 4, 2)), Console.out )

  FluidicParser.parse(input) foreach (op => interp.perform(op))
}