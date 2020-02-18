organization := "net.invalidkeyword"

name := "scaladiagrams"

version := "1.1"

scalaVersion := "2.13.1"

scalacOptions ++= Seq("-unchecked", "-deprecation")

libraryDependencies ++= Seq(
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2",
  "org.scalatest" % "scalatest_2.13" % "3.1.0" % "test",
  "org.rogach" %% "scallop" % "3.3.2"
)

test in assembly := {}

jarName in assembly := "scaladiagrams-assembly-1.1.jar"

resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/"

resolvers += "Typesafe Repository" at "https://repo.typesafe.com/typesafe/releases/"


