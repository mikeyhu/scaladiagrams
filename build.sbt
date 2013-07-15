organization := "net.invalidkeyword"

name := "scaladiagrams"

version := "1.0"

scalaVersion := "2.9.2"

scalacOptions ++= Seq("-unchecked", "-deprecation")

seq(assemblySettings: _*)

libraryDependencies ++= Seq(
  "org.scalatest" % "scalatest_2.9.0" % "1.6.1" % "test",
  "org.rogach" %% "scallop" % "0.6.0"
)

resolvers += "Sonatype OSS Snapshots" at "http://oss.sonatype.org/content/repositories/snapshots/"

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"


