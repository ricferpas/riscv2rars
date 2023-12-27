//val myMainClass = Some("mips2rars.Test")
val myMainClass = Some("mips2rars.Main")

Compile /  run / mainClass := myMainClass

Compile / packageBin / mainClass := myMainClass

name := "mips2rars"

version := "0.1-SNAPSHOT"

scalaVersion := "2.13.10"

scalacOptions ++= Seq("-deprecation", "-feature", "-unchecked")

Compile / doc / scalacOptions ++= Opts.doc.title("Mips2Rars") ++ Seq("-diagrams")

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2"

// For being able to set java memory options: 

fork := true

outputStrategy := Some(StdoutOutput)

run / connectInput := true

javaOptions ++= Seq("-Xms1024m", "-Xmx2048m")
