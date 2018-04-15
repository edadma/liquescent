package xyz.hyperreal.fluidic


object Main extends App {

//  val parser = new FluidicParser
  val input = """asdf {{ title }} zxcv {% tag1 %} qwer {% tag2 %}"""

//  println( parser( input ) )

  println( FluidicParser.elements( input ) )
}