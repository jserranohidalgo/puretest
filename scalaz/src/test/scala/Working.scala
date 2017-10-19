package org.hablapps.puretest
package test

import scalaz.{MonadState, MonadError}
import scalaz.syntax.monadError._
import Filter.syntax._

/** Programs */

import WorkingProgram.Error

trait WorkingProgram[P[_]]{

  val MS: MonadState[P,Int]
  implicit val ME: MonadError[P,Error]

  def workingProgram: P[Unit] =
    ().point[P]

  def workingProgramWithHandledError: P[Error] =
    Error(1).raiseError[P,Error] handleError {
      _.point[P]
    }

  def failingProgram: P[Unit] =
    ME.raiseError(Error(0))
}

object WorkingProgram{
  case class Error(i: Int)

  def apply[P[_]](implicit _MS: MonadState[P,Int],
    _ME: MonadError[P,Error]) = new WorkingProgram[P]{
    val MS = _MS
    val ME = _ME
  }
}