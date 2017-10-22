package org.hablapps.puretest
package test

import scalaz.{MonadState, MonadError}
import scalaz.syntax.monadError._
import Filter.syntax._

import WorkingProgram.Error

trait WorkingSpec[P[_]] extends FunSpec[P] {
  val S: WorkingProgram[P]
  import S.{ME => _, _}
  implicit val ME: MonadError[P, PureTestError[Error]]

  Describe("ShouldSucceed"){

    It("should work with working programs"){
      workingProgram.shouldSucceed >>
      workingProgramWithHandledError.shouldSucceed
    }

    It("is redundant with working programs"){
      workingProgram >>
      workingProgramWithHandledError
    }

    It("should work when the exact value is checked"){
      workingProgram.shouldBe(1)
    }

    It("should work if patterns are matched"){
      (workingProgramWithHandledError shouldMatch { _ == 2 }) >>
      ((MS.put(2) >> MS.get) shouldMatch { _ == 2 })
    }

    It("should work if patterns are matched (for-comprehension)"){
      (for {
        2 <- workingProgramWithHandledError
      } yield ()) >>
      (for {
        _ <- MS.put(2)
        2 <- MS.get
      } yield ())
    }
  }

  Describe("ShouldFail"){

    It("should work with failing programs"){
      failingProgram shouldFail
    }

    It("should work when the exact error is checked"){
      failingProgram shouldFail(Error(0))
    }

    It("should work if error pattern is matched"){
      failingProgram shouldMatchFailure( _ == Error(0))
    }
  }

  Describe("ShouldFail over puretest programs"){
    It("shouldn't work with working programs"){
      workingProgram.shouldFail // shouldFail
    }

    It("shouldn't work with working programs (II)"){
      (for {
        2 <- workingProgramWithHandledError
      } yield ()).shouldFail
    }

    It("should fail if it doesn't pattern match"){
      (for {
        _ <- MS.put(1)
        2 <- MS.get
      } yield ()) // .shouldFail
    }
  }
}

object WorkingSpec{
  class Scalatest[P[_]](
    val S: WorkingProgram[P],
    val ME: MonadError[P,PureTestError[Error]],
    val Tester: Tester[P,PureTestError[Error]])
  extends scalatestImpl.ScalatestFunSpec[P,Error]
  with WorkingSpec[P]
}



