package net.invalidkeyword.scaladiagrams

import org.scalatest.{FlatSpec, Matchers}

class NodeSelectorTests extends FlatSpec with Matchers {

  "The NodeSelector" should " find a node if one exists if sent a String" in {
    	val nodes = List(CLASS("myClass",List()), CLASS("anotherClass",List()))
    	val ns = new NodeSelector(nodes)
    	ns.findNode("myClass") should be(Some(CLASS("myClass",List())))
    }
    
    it should " find a node if one exists if being sent a WITH" in {
    	val nodes = List(CLASS("myClass",List()), CLASS("anotherClass",List()))
    	val ns = new NodeSelector(nodes)
    	ns.findNode(RELATED("myClass")) should be(Some(CLASS("myClass",List())))
    }    
    
    it should " not find node if it does not exist" in {
    	val nodes = List(CLASS("myClass",List()), CLASS("anotherClass",List()))
    	val ns = new NodeSelector(nodes)
    	ns.findNode(RELATED("abc")) should be(None)
    }
    
    it should " return a set of all child nodes for a node" in {
        val nodes = List(CLASS("myClass",List(RELATED("anotherClass"))), CLASS("anotherClass",List(RELATED("third"))), CLASS("unused",List()), CLASS("third",List()))
    	val ns = new NodeSelector(nodes)
        val node = ns.findNode("myClass").get
        ns.selectChildNodes(node) should be(Set(CLASS("third",List()),CLASS("myClass",List(RELATED("anotherClass"))),CLASS("anotherClass",List(RELATED("third")))))   
    }
 
}