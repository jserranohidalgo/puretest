package org.hablapps.puretest
package test

import scalaz.{MonadState, MonadError}
import scalaz.syntax.monad._
import Filter.syntax._

trait BooleanPrograms[P[_]]{

  val MS: MonadState[P, Int]
  implicit val ME: MonadError[P, Throwable]
  implicit val RE: RaiseError[P, PureTestError[Throwable]]

  def trueProgram: P[Boolean] = for {
    _ <- MS.put(1)
    1 <- MS.get
  } yield true

  def falseProgram: P[Boolean] =
    false.point[P]

  def failingMatchBoolProgram: P[Boolean] = for {
    _ <- MS.put(1)
    2 <- MS.get
  } yield true

  def raisedErrorBoolProgram: P[Boolean] =
    ME.raiseError(new RuntimeException("forced exception"))
}

object BooleanPrograms{
  def apply[P[_]](implicit _MS: MonadState[P,Int], 
    _ME: MonadError[P,Throwable],
    _RE: RaiseError[P,PureTestError[Throwable]]) =
    new BooleanPrograms[P]{
      val MS = _MS
      val ME = _ME
      val RE = _RE
    }
}
