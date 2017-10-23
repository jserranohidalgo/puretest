package org.hablapps
package puretest
package test

import org.scalatest._
import puretest.{Filter => TestFilter}, scalatestImpl._

import scalaz._, Scalaz._

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