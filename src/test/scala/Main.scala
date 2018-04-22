package xyz.hyperreal.liquescent


object Main extends App {

//  val parser = new LiquescentParser
  val input =
		"""
		  {% capture my_variable %}
				{% assign a = 3 %}
				{% if a == 1 %}
					one
				{% elsif a == 2 %}
					two
				{% elsif a == 3 %}
					three
				{% else %}
					something else
				{% endif %}
	 		{% endcapture %}
			asdf {{ my_variable }}
    """

//  println( parser( input ) )

  val interp = new Interpreter( StandardFilters.map, Map("article" -> Map("published_at" -> "2015-07-17"), "product_price" -> 1.49, "list" -> List(Map("a" -> "asdf"), 3, 2, Map("b" -> "oops"), 4, Map("a" -> "qwer"))) )

//	println( LiquescentParser.parse(input) )
  interp.perform( LiquescentParser.parse(input), Console.out )
	println
}