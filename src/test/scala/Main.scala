package xyz.hyperreal.fluidic


object Main extends App {

//  val parser = new FluidicParser
  val input = """asdf {{ data[5].asdf["title"] }} zxcv"""

//  println( parser( input ) )

  println( FluidicParser.parse(input) )
}