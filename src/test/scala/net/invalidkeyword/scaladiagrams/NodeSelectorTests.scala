package net.invalidkeyword.scaladiagrams

import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers

class NodeSelectorTests extends Spec with ShouldMatchers  {

  describe("The NodeSelector") {
    it("should find a node if one exists if sent a String") {
    	val nodes = List(CLASS("myClass",List()), CLASS("anotherClass",List()))
    	val ns = new NodeSelector(nodes)
    	ns.findNode("myClass") should be(Some(CLASS("myClass",List())))
    }
    
    it("should find a node if one exists if being sent a WITH") {
    	val nodes = List(CLASS("myClass",List()), CLASS("anotherClass",List()))
    	val ns = new NodeSelector(nodes)
    	ns.findNode(WITH("myClass")) should be(Some(CLASS("myClass",List())))
    }    
    
    it("should not find node if it does not exist") {
    	val nodes = List(CLASS("myClass",List()), CLASS("anotherClass",List()))
    	val ns = new NodeSelector(nodes)
    	ns.findNode(WITH("abc")) should be(None)
    }
    
    it("should return a set of all child nodes for a node") {
        val nodes = List(CLASS("myClass",List(WITH("anotherClass"))), CLASS("anotherClass",List(WITH("third"))), CLASS("unused",List()), CLASS("third",List()))
    	val ns = new NodeSelector(nodes)
        val node = ns.findNode("myClass").get
        ns.selectNodes(node) should be(Set(CLASS("third",List()),CLASS("myClass",List(WITH("anotherClass"))),CLASS("anotherClass",List(WITH("third")))))
        
    }
  }
}