package org.hablapps
package puretest
package test


import scalaz._

import WorkingProgram.Error, ShouldSpecStateT.Program

class ShouldSpecStateT extends ShouldSpec.Scalatest[Program](
  WorkingProgram[Program](implicitly,
    PureTestError.toMonadError(MonadError[Program,PureTestError[Error]])),
  RaiseError[Program,PureTestError[Error]],
  HandleError[Program,PureTestError[Error]],
  RaiseError[Program,PureTestError[PureTestError[Error]]],
  StateTester[Program,Int,PureTestError[PureTestError[Error]]].apply(0)
)

object ShouldSpecStateT{
  type Program[T] = StateT[PureTestError[PureTestError[Error]] \/ ?, Int, T]
}