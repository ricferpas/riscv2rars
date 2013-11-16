import sbt._
import sbt.Keys._
import com.typesafe.sbteclipse.plugin.EclipsePlugin.EclipseKeys
import com.typesafe.sbteclipse.plugin.EclipsePlugin.EclipseCreateSrc
import ProguardPlugin._

object ProjectBuild extends Build {
  //val myMainClass = Some("mips2mars.Test")
  val myMainClass = Some("mips2mars.Main")

  lazy val proguard = proguardSettings ++ Seq(
    proguardOptions := Seq(keepMain("mips2mars.Main"))
  )

  lazy val root = Project(
    id = "root",
    base = file("."),
    settings = Project.defaultSettings ++ Seq(
      name := "mips2mars",
      organization := "org.example",
      version := "0.1-SNAPSHOT",
      scalaVersion := "2.10.2",
      scalacOptions ++= Seq("-deprecation", "-feature", "-unchecked", "-explaintypes"),
      scalacOptions in (Compile, doc) ++= Opts.doc.title("Mips2Mars") ++ Seq("-diagrams"),
      //libraryDependencies <+= scalaVersion { "org.scala-lang" % "scala-compiler" % _ % "compile" },
      //libraryDependencies <+= scalaVersion { "org.scala-lang" % "scala-swing" % _ },
      mainClass in (Compile, run) := myMainClass,
      mainClass in (Compile, packageBin) := myMainClass)).
  settings(proguard: _*)

  EclipseKeys.withSource := true
}
