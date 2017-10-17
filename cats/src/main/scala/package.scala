package org.hablapps

package object puretest
  extends StateValidatedMonad
  with TestingOps
  with Errors {

	type Location = (sourcecode.File, sourcecode.Line)

	def simplifyLocation(location: Location): String = {
	  val fileext = raw".*/(.*)".r
	  val fileext(filename) = location._1.value
	  s"($filename:${location._2.value})"
	}

}