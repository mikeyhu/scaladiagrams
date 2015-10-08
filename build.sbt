organization := "net.invalidkeyword"

name := "scaladiagrams"

version := "1.0"

scalaVersion := "2.11.7"

scalacOptions ++= Seq("-unchecked", "-deprecation")

libraryDependencies ++= Seq(
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.2",
  "org.scalatest" % "scalatest_2.11" % "2.2.4" % "test",
  "org.rogach" %% "scallop" % "0.9.5"
)

test in assembly := {}

jarName in assembly := "scaladiagrams-assembly-1.0.jar"

resolvers += "Sonatype OSS Snapshots" at "http://oss.sonatype.org/content/repositories/snapshots/"

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"


