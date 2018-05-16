Liquescent
==========

[![Build Status](https://www.travis-ci.org/edadma/liquescent.svg?branch=master)](https://www.travis-ci.org/edadma/liquescent)
[![Coverage Status](https://coveralls.io/repos/github/edadma/liquescent/badge.svg?branch=master)](https://coveralls.io/github/edadma/liquescent?branch=master)
[![License](https://img.shields.io/badge/license-ISC-blue.svg)](https://github.com/edadma/liquescent/blob/master/LICENSE)
[![Version](https://img.shields.io/badge/latest_release-v0.2-orange.svg)](https://github.com/edadma/liquescent/releases/tag/v0.2)

*Liquescent* is an implementation of the [Liquid](https://shopify.github.io/liquid/) templating language for the [Scala](http://scala-lang.org) programming language.


Examples
--------

### Library

This example program shows how to create a custom tag to output an HTML ordered list.

```scala
import java.io.PrintStream

import scala.collection.mutable

import xyz.hyperreal.liquescent._


object Example extends App {

  val input =
    """
      |<h2>Vaudeville Acts</h2>
      |<ol>
      |  {% for act in acts %}
      |    <li>
      |      <h3>{{ act.name }}</h3>
      |      {% ul act.members %}
      |    </li>
      |  {% endfor %}
      |</ol>
    """.trim.stripMargin
  val acts =
    List(
      Map(
        "name" -> "Three Stooges",
        "members" -> List( "Larry", "Moe", "Curly" )
      ),
      Map(
        "name" -> "Andrews Sisters",
        "members" -> List( "LaVerne", "Maxine", "Patty" )
      ),
      Map(
        "name" -> "Abbott and Costello",
        "members" -> List( "William (Bud) Abbott", "Lou Costello" )
      )
    )
  val customtag =
    new Tag( "ul" ) {
      def apply( settings: Map[Symbol, Any], vars: mutable.Map[String, Any], out: PrintStream, args: List[Any], context: AnyRef ) = {
        val list = args.head.asInstanceOf[List[String]]

        out.print(s"<ul>${list map (item => s"<li>$item</li>") mkString}</ul>")
      }
    }

  new Interpreter( StandardFilters.map, Tag(customtag), Map(), Map("acts" -> acts), null ).
    render( LiquescentParser.parse(io.Source.fromString(input)), Map(), Console.out, false )
}
```

This program prints

```html
<h2>Vaudeville Acts</h2>
<ol>

    <li>
      <h3>Three Stooges</h3>
      <ul><li>Larry</li><li>Moe</li><li>Curly</li></ul>
    </li>

    <li>
      <h3>Andrews Sisters</h3>
      <ul><li>LaVerne</li><li>Maxine</li><li>Patty</li></ul>
    </li>

    <li>
      <h3>Abbott and Costello</h3>
      <ul><li>William (Bud) Abbott</li><li>Lou Costello</li></ul>
    </li>

</ol>
```

### Executable

This next example shows how to use *Liquescent* as an executable on the command line.

```bash
echo "{{ v | join: \", \" }}" | java -jar liquescent-0.2.jar -j "{v: [\"one\", \"two\", \"three\"]}" --
```

The above command prints

    one, two, three


Usage
-----

Use the following definition to use Liquescent in your Maven project:

```xml
<repository>
  <id>hyperreal</id>
  <url>https://dl.bintray.com/edadma/maven</url>
</repository>

<dependency>
  <groupId>xyz.hyperreal</groupId>
  <artifactId>liquescent</artifactId>
  <version>0.2</version>
</dependency>
```

Add the following to your `build.sbt` file to use Liquescent in your SBT project:

```sbt
resolvers += "Hyperreal Repository" at "https://dl.bintray.com/edadma/maven"

libraryDependencies += "xyz.hyperreal" %% "liquescent" % "0.2"
```

Building
--------

### Requirements

- Java 8
- SBT 1.1.5+
- Scala 2.12.6+

### Clone and Assemble Executable

```bash
git clone git://github.com/edadma/liquescent.git
cd liquescent
sbt assembly
```

The command `sbt assembly` also runs all the unit tests.


License
-------

ISC © 2018 Edward Maxedon