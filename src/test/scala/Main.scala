package xyz.hyperreal.fluidic


object Main extends App {

//  val parser = new FluidicParser
  val input =
    """
      |{{ "Have <em>you</em> read <strong>Ulysses</strong>?" | strip_html }}|
    """

//  println( parser( input ) )

  val interp = new Interpreter( StandardFilters.map, Map("list" -> List(Map("a" -> "asdf"), 3, 2, Map("b" -> "oops"), 4, Map("a" -> "qwer"))), Console.out )

  FluidicParser.parse(input) foreach (op => interp.perform(op))
}