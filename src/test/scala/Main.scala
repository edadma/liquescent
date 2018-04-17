package xyz.hyperreal.fluidic


object Main extends App {

//  val parser = new FluidicParser
  val input = """asdf {{ title }} zxcv {% comment %} qwer {% endcomment %} {%raw%}{% coocoo %}{%endraw%} {% wow %}"""

//  println( parser( input ) )

  println( FluidicParser.elements(input) filterNot new CommentFilter flatMap new RawTransform )
}