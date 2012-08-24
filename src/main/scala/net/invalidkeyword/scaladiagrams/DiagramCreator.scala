package net.invalidkeyword.scaladiagrams

import java.io.File

object DiagramCreator { 
  

  def main(args:Array[String]) = {
    val inputFinder = new InputFinder()
    
    val nodes = inputFinder.files("/home/casper/casper/core/src/main/scala").map (a=> parseFile(a)).flatten.toList
    val platform = inputFinder.files("/home/casper/casper/core/platform/src/main/scala").map(a=> parseFile(a)).flatten.toList
    val ns = new NodeSelector(nodes ++ platform)
   
    val root = ns.findNode("Controllers").get
    ns.selectNodes(root).foreach( println _)
    
    //ns.nodesWithParentsOrChildren.foreach( println _)
  }
  
  
  
  def fileToString(file : File) = {scala.io.Source.fromFile(file).mkString}
  
  def parseFile(file : File) = {
    val result = ScalaSourceParser.run(fileToString(file))
    ScalaSourceParser.filter(result.get)
  }
}