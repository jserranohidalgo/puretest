package org.hablapps.puretest

import scalaz.{Functor, Monad, MonadError}
import scalaz.syntax.monadError._

class PureMatchers[P[_], A, E](self: P[A])(implicit
  M: Monad[P],
  HE: HandleError[P, E],
  RE: RaiseError[P, PureTestError[E]],
  loc: Location){

  // Basic

  private def shouldFail(p: E => Boolean,
    errorIfSuccess: A => PureTestError[E],
    errorIfFailure: E => PureTestError[E]): P[Unit] =
    HE.handleError(
      self.flatMap{ a: A => 
        RE.raiseError[Unit](errorIfSuccess(a))
    }){
      case error =>
        if (p(error)) ().point[P]
        else RE.raiseError(errorIfFailure(error))
    }

  private def shouldSucceed(p: A => Boolean,
    errorIfSuccess: A => PureTestError[E],
    errorIfFailure: E => PureTestError[E]): P[A] =
    HE.handleError(self){
      case error =>
        RE.raiseError[A](errorIfFailure(error))
    }.flatMap{
      a => if (p(a)) a.point[P]
        else RE.raiseError[A](errorIfSuccess(a))
    }

  // Failure

  def shouldMatchFailure(p: E => Boolean): P[Unit] =
    shouldFail(p, NotFailed(_), NotMatchedFailure(_))

  def shouldFail: P[Unit] =
    shouldFail(_ => true, NotFailed(_), _ => ShouldNotHappen())

  def shouldFail(e: E): P[Unit] =
    shouldFail(_ == e, NotError(_, e), OtherError(_, e))

  // Success

  def shouldMatch(p: A => Boolean): P[A] =
    shouldSucceed(p, NotMatched(_), NotSucceeded(_))

  def shouldBe(a: A): P[A] =
    shouldSucceed(_ == a, NotEqualTo(_, a), NotValue(_, a))

  def shouldSucceed: P[A] =
    shouldSucceed(_ => true, _ => ShouldNotHappen(), NotSucceeded(_))
}
