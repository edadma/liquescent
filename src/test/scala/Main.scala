package xyz.hyperreal.fluidic


object Main extends App {

//  val parser = new FluidicParser
  val input =
		"""
			{% assign a = true %}
	 		{% if a %}
				yes
			{% else %}
	 			no
			{% endif %}
			asdf
    """

//  println( parser( input ) )

  val interp = new Interpreter( StandardFilters.map, Map("article" -> Map("published_at" -> "2015-07-17"), "product_price" -> 1.49, "list" -> List(Map("a" -> "asdf"), 3, 2, Map("b" -> "oops"), 4, Map("a" -> "qwer"))), Console.out )

//	println( FluidicParser.parse(input) )
  interp.perform( FluidicParser.parse(input) )
	println
}