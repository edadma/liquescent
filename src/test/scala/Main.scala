package xyz.hyperreal.fluidic


object Main extends App {

//  val parser = new FluidicParser
  val input =
    """
      |{{ "John, Paul, George, Ringo" | split: ", " }}|
    """

//  println( parser( input ) )

  val interp = new Interpreter( BuiltinFilters.map, Map("list" -> List(Map("a" -> "asdf"), 3, 2, Map("b" -> "oops"), 4, Map("a" -> "qwer"))), Console.out )

  FluidicParser.parse(input) foreach (op => interp.perform(op))
}