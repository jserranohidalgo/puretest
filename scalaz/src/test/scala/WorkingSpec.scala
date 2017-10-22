package org.hablapps.puretest
package test

import scalaz.{MonadState, MonadError}
import scalaz.syntax.monadError._
import Filter.syntax._

import WorkingProgram.Error

trait WorkingSpec[P[_]] extends FunSpec[P] {
  val S: WorkingProgram[P]
  import S._
  implicit val RE: RaiseError[P, PureTestError[Error]]

  Describe("Working programs"){

    It("should pass if work"){
      workingProgram.shouldSucceed >>
      workingProgramWithHandledError.shouldSucceed
    }

    It("should fail if it doesn't work"){
      failingProgram.shouldFail
    }

    It("should fail if it desn't pattern match"){
      (for {
        Error(2) <- workingProgramWithHandledError
      } yield ()).shouldFail >>
      (for {
        _ <- MS.put(1)
        2 <- MS.get
      } yield ()).shouldFail
    }
  }
}

object WorkingSpec{
  class Scalatest[P[_]](
    val S: WorkingProgram[P],
    val RE: RaiseError[P,PureTestError[Error]],
    val Tester: Tester[P,PureTestError[Error]])
  extends scalatestImpl.ScalatestFunSpec[P,Error]
  with WorkingSpec[P]
}



