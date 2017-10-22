package org.hablapps
package puretest
package test

import org.scalatest._
import puretest.{Filter => TestFilter}, scalatestImpl._

import scalaz._, Scalaz._

import WorkingProgram.Error, WorkingSpecStateT.Program

class WorkingSpecStateT extends WorkingSpec.Scalatest[Program](
  WorkingProgram[Program](implicitly, toMonadError[Program,Error,PureTestError[Error]]),
  RaiseError[Program,PureTestError[Error]],
  StateTester[Program,Int,PureTestError[Error]].apply(0)
)

object WorkingSpecStateT{
  type Program[T] = StateT[PureTestError[Error] \/ ?, Int, T]
}