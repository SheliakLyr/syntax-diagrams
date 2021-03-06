Sequence Diagrams
=======

This library is used to generate syntax diagrams (https://en.wikipedia.org/wiki/Syntax_diagram) from Scala code. 
Features:
* Grammar is defined directly in Scala
* Diagrams are generated as SVG files:
  * Can be styled via CSS
  * Non-terminals can be rendered as links to separate diagrams 
  
Usage
-----

Diagrams are represented by type Expr. You can use its subclasses 
to construct primitives and combine them into larger expressions:
* Symbols
  * `Terminal(value: String)`
  * `NonTerminal(value: String)`
  * `Special(value: String)` - special kind of terminal. For example: textual description, regexp etc.
* Combinators
  * `Repeat(e: Expr)` - at least one e
  * `Optional(e: Expr)` - e or nothing
  * `Choice(es: Seq[Expr])` - selects one of the alternatives
  * `Sequence(es: Seq[Expr])` - joins es in a sequence (concatenation)

Diagrams are generated by the method `SVG.createDiagram`:
```scala
def createDiagram(rhs: Expr, options: Options = Options()): Tag
```

You can specify the following options:
* `maxWidth` - maximal width of the diagram. This is **not** a hard constraint - the generator does its best to fit the diagram but it may fail to do so.
* `cssClass` - css class name used in generated SVG diagram
* `embeddedStyle` - CSS to embed directly in SVG
* `linker: String => String` - method used to convert NonTerminals to HTML links. For example `"#" + _`. By default does nothing (_ => "").
* `showNonTerm: String => String` - method used to convert NonTerminals to HTML links. For example `"#" + _`. By default does nothing (_ => "").

Example
-------

See [JsonExampleRunner.scala](src/test/scala/com/github/sheliaklyr/ebnf/JsonExampleRunner.scala)
for example that generates a single html page with syntax diagrams for JSON. 
You can see the generated page here: 
[JSON Syntax](https://sheliaklyr.github.io/syntax-diagrams/) 