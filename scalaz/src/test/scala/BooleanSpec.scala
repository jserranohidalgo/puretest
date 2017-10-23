package org.hablapps.puretest
package test


trait BooleanSpec[P[_]] extends FunSpec[P] {
  val S: BooleanPrograms[P]
  import S._

  Describe("Boolean programs"){
    Holds("if return true"){
      trueProgram
    }

    Holds("if not return false"){
      falseProgram not
    }

    Holds("AndThen"){
      (trueProgram andThen trueProgram) andThen
      (trueProgram andThen falseProgram).not andThen
      (falseProgram andThen trueProgram).not andThen
      (falseProgram andThen falseProgram).not
    }

    Holds("OrElse"){
      (trueProgram orElse trueProgram) andThen
      (trueProgram orElse falseProgram) andThen
      (falseProgram orElse trueProgram) andThen
      (falseProgram orElse falseProgram).not
    }

    Holds("Implies"){
      (trueProgram implies trueProgram) andThen
      (trueProgram implies falseProgram).not andThen
      (falseProgram implies trueProgram) andThen
      (falseProgram implies falseProgram)
    }

    // Holds("not holds if program fails"){
    //   // raisedErrorBoolProgram
    // }
  }
}

object BooleanSpec{
  class Scalatest[P[_]](
    val S: BooleanPrograms[P],
    val Tester: Tester[P,PureTestError[Throwable]])
  extends scalatestImpl.ScalatestFunSpec[P,Throwable] with BooleanSpec[P]
}