package xyz.hyperreal.fluidic


object Main extends App {

//  val parser = new FluidicParser
  val input =
    """
      |{{ "1 < 2 & 3" | escape_once | escape_once }}|
    """

//  println( parser( input ) )

  val interp = new Interpreter( StandardFilters.map, Map("product_price" -> 1.49, "list" -> List(Map("a" -> "asdf"), 3, 2, Map("b" -> "oops"), 4, Map("a" -> "qwer"))), Console.out )

  FluidicParser.parse(input) foreach (op => interp.perform(op))
}