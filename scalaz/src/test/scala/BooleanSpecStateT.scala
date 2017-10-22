package org.hablapps
package puretest
package test

import org.scalatest._
import puretest.{Filter => TestFilter}, scalatestImpl._

import scalaz._, Scalaz._
import BooleanSpecStateT.Program

class BooleanSpecStateT extends BooleanSpec.Scalatest[Program](
  BooleanPrograms[Program](implicitly, toMonadError[Program,Throwable,PureTestError[Throwable]], implicitly),
  StateTester[Program,Int,PureTestError[Throwable]].apply(0)
)

object BooleanSpecStateT{
  type Program[T] = StateT[PureTestError[Throwable] \/ ?, Int, T]
}