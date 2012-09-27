Scala Diagrams
==============
Scaladiagrams is a command line tool to generate DOT files representing a Scala projects class hierarchy. These DOT files can then be passed to a renderer
such as [GraphViz](http://www.graphviz.org/) to draw the hierarchy.

Usage
-----
First, compile and package with ./build 

To run Scaladiagrams do the following:

	scaladiagrams --source pathToScalaSourcefiles > dotFile

If you have Graphviz installed, you can generate an image like so:

	scaladiagrams --source pathToScalaSourcefiles | dot -Tpng > file.png

Large class hierarchies are probably best rendered as an SVG file, just replace the -t target to svg.

Other options
-------------

	./scaladiagrams --help
		-e, --extension  <arg>    (default = .scala) 
		-l, --linked             only output types that extend other types 
		-s, --source  <arg>      location of source files (default = .)

Most useful of these is -l, to remove types that do not inherit anything and are not inherited.

Example diagram
---------------
![Class Diagram](https://raw.github.com/mikeyhu/scaladiagrams/master/example-output/diagram.png)