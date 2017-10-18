package org.hablapps.puretest

trait FilterSpec[P[_]] extends Filter.Syntax {

  implicit val Fi: Filter[P]

}
