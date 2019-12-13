organization := "com.github.sheliaklyr"

name := "syntax-diagrams"

version := "0.1-SNAPSHOT"

scalaVersion := "2.12.10"

triggeredMessage := Watched.clearWhenTriggered

libraryDependencies += "com.lihaoyi" %% "scalatags" % "0.7.0"

libraryDependencies += "com.lihaoyi" %% "fastparse" % "0.4.4"

libraryDependencies += "com.lihaoyi" %% "pprint" % "0.5.6"

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.5"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"
    
