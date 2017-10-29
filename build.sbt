organization := "com.github.sheliaklyr"

name := "syntax-diagrams"

version := "0.1-SNAPSHOT"

scalaVersion := "2.11.8"

triggeredMessage := Watched.clearWhenTriggered

libraryDependencies += "com.lihaoyi" %% "scalatags" % "0.6.5"

libraryDependencies += "com.lihaoyi" %% "fastparse" % "0.4.3"

libraryDependencies += "com.lihaoyi" %% "pprint" % "0.4.3"

libraryDependencies += "org.scala-lang.modules" % "scala-parser-combinators_2.11" % "1.0.5"

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "3.0.1" % "test"
    
