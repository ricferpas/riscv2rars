//val myMainClass = Some("riscv2rars.Test")
val myMainClass = Some("riscv2rars.Main")

Compile /  run / mainClass := myMainClass

Compile / packageBin / mainClass := myMainClass

name := "riscv2rars"

version := "0.1-SNAPSHOT"

scalaVersion := "3.7.4"

scalacOptions ++= Seq("-deprecation", "-feature", "-unchecked")

Compile / doc / scalacOptions ++= Opts.doc.title("Riscv2Rars") ++ Seq("-diagrams")

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "2.4.0"
