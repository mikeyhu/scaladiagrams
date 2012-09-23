package net.invalidkeyword.scaladiagrams

import java.io.File
import org.rogach.scallop._

object DiagramCreator { 
  
  def main(args:Array[String]) = {
    
    object Config extends ScallopConf(args) {
      val extension = opt[String]("extension", default=Some(".scala"))
      val source = opt[String]("source", default=Some("."), descr = "location of source files")
      val linked = opt[Boolean]("linked", descr = "only output types that extend other types")
    }
    
    val nodes = new InputFinder().files(Config.source(),Config.extension()).map(a=> parseFile(a)).flatten.toList
    val ns = new NodeSelector(nodes)
     
    val selectedNodes = if(Config.linked())
      ns.nodesWithParentsOrChildren
    else
      nodes
   
    println ("digraph diagram {")
    selectedNodes.foreach(println _)
    println ("}")
  }
  
  def fileToString(file : File) = {scala.io.Source.fromFile(file).mkString}
  
  def parseFile(file : File) = {
    val result = ScalaSourceParser.run(fileToString(file))
    ScalaSourceParser.filter(result.get)
  }
}