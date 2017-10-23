package org.hablapps
package puretest
package test


import scalaz._

import WorkingProgram.Error, WorkingSpecStateT.Program

class WorkingSpecStateT extends WorkingSpec.Scalatest[Program](
  WorkingProgram[Program],
  RaiseError[Program,PureTestError[Error]],
  StateTester[Program,Int,PureTestError[Error]].apply(0)
)

object WorkingSpecStateT{
  type Program[T] = StateT[PureTestError[Error] \/ ?, Int, T]
}