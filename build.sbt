organization := "com.github.sheliaklyr"

name := "syntax-diagrams"

version := "0.1-SNAPSHOT"

scalaVersion := "2.13.15"

triggeredMessage := Watched.clearWhenTriggered

libraryDependencies += "com.lihaoyi" %% "scalatags" % "0.13.1"

libraryDependencies += "com.lihaoyi" %% "fastparse" % "3.1.1"

libraryDependencies += "com.lihaoyi" %% "pprint" % "0.9.0"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.19" % "test"
    
crossScalaVersions := Seq("2.13.15", "2.12.20")
