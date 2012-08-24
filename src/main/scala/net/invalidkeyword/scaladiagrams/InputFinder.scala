package net.invalidkeyword.scaladiagrams

import java.io.File

class InputFinder {
	def tree(root: File, skipHidden: Boolean = false): Stream[File] = 
	  if (!root.exists || (skipHidden && root.isHidden)) Stream.empty 
	  else root #:: (
	    root.listFiles match {
	      case null => Stream.empty
	      case files => files.toStream.flatMap(tree(_, skipHidden))
	  })
	  
	def files(dir : String) = tree(new File(dir)).filter(f => f.isFile && f.getName.endsWith(".scala")).toArray
}