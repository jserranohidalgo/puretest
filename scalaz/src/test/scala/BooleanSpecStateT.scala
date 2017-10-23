package org.hablapps
package puretest
package test


import scalaz._
import BooleanSpecStateT.Program

class BooleanSpecStateT extends BooleanSpec.Scalatest[Program](
  implicitly,
  implicitly,
  implicitly,
  implicitly,
  StateTester[Program,Int,PureTestError[Throwable]].apply(0)
)

object BooleanSpecStateT{
  type Program[T] = StateT[PureTestError[Throwable] \/ ?, Int, T]
}