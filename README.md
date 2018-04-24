liquescent
==========

[![Build Status](https://www.travis-ci.org/edadma/liquescent.svg?branch=master)](https://www.travis-ci.org/edadma/liquescent)
[![Coverage Status](https://coveralls.io/repos/github/edadma/liquescent/badge.svg?branch=master)](https://coveralls.io/github/edadma/liquescent?branch=master)
[![License](https://img.shields.io/badge/license-ISC-blue.svg)](https://opensource.org/licenses/ISC)
[![Version](https://img.shields.io/badge/latest_release-v0.1.1-orange.svg)](https://github.com/edadma/liquescent/releases/tag/v0.1.1)

*liquescent* is an implementation of the [Liquid](https://shopify.github.io/liquid/) templating language for the [Scala](http://scala-lang.org) programming language.


Examples
--------

### Library

This example program shows how to create a custom tag to output an HTML ordered list.

	import java.io.PrintStream

	import scala.collection.mutable

	import xyz.hyperreal.liquescent._


	object Example extends App {

      val input =
        """
          |{% assign variable = "stupider" %}
          |{% ol "stupid", variable, "stupidest" %}
        """.trim.stripMargin

      val customtag =
        new Tag( "ol" ) {
          def apply(vars: mutable.Map[String, Any], out: PrintStream, args: List[Any]) =
            out.print( s"<ol>${args map (item => s"<li>$item</li>") mkString}</ol>" )
        }

      new Interpreter( StandardFilters.map, Tag.map(customtag), Map() ).perform( LiquescentParser.parse(input), Console.out )
	}

This program prints

	<ol><li>stupid</li><li>stupider</li><li>stupidest</li></ol>


### Executable

This next example shows how to use *liquescent* as an executable on the command line.

	echo "{{ v | join: \", \" }}" | java -jar liquescent-0.1.1.jar -j "{v: [\"one\", \"two\", \"three\"]}" --

The above command prints

	one, two, three


Usage
-----

Use the following definition to use *liquescent* in your Maven project:

	<repository>
	  <id>hyperreal</id>
	  <url>https://dl.bintray.com/edadma/maven</url>
	</repository>

	<dependency>
	  <groupId>xyz.hyperreal</groupId>
	  <artifactId>liquescent</artifactId>
	  <version>0.1.1</version>
	</dependency>

Add the following to your `build.sbt` file to use *liquescent* in your SBT project:

	resolvers += "Hyperreal Repository" at "https://dl.bintray.com/edadma/maven"

	libraryDependencies += "xyz.hyperreal" %% "liquescent" % "0.1.1"


Building
--------

### Requirements

- Java 8
- SBT 1.1.4+
- Scala 2.12.5+

### Clone and Assemble Executable

	git clone git://github.com/edadma/liquescent.git
	cd liquescent
	sbt assembly

The command `sbt assembly` also runs all the unit tests.


License
-------

ISC © 2018 Edward Maxedon