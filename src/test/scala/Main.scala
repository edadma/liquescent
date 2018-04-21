package xyz.hyperreal.fluidic


object Main extends App {

//  val parser = new FluidicParser
  val input =
    """
			||{{ "Have you read 'James & the Giant Peach'?" | escape }}|
    """

//  println( parser( input ) )

  val interp = new Interpreter( StandardFilters.map, Map("article" -> Map("published_at" -> "2015-07-17"), "product_price" -> 1.49, "list" -> List(Map("a" -> "asdf"), 3, 2, Map("b" -> "oops"), 4, Map("a" -> "qwer"))), Console.out )

  FluidicParser.parse(input) foreach (op => interp.perform(op))
}