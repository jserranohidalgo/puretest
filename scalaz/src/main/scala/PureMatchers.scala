package org.hablapps.puretest

import scalaz.{Functor, Monad, MonadError}
import scalaz.syntax.monadError._

class PureMatchers[P[_], A, E](self: P[A])(implicit
  ME: MonadError[P, PureTestError[E]],
  loc: Location){

  def should(condition: Either[E,A] => Boolean,
    errorIfSuccess: A => PureTestError[E],
    errorIfFailure: E => PureTestError[E]): P[Unit] =
    self.attempt(PureTestError.toMonadErrorApp).map{ result =>
      if (condition(result)) ().pure[P]
      else ME.raiseError(result.fold(errorIfFailure,errorIfSuccess))
    }

  def shouldFail: P[Unit] =
    should(_.isLeft, NotFailed(_)(loc), _ => ShouldNotHappen(loc))

  def shouldSucceed: P[Unit] =
    should(_.isRight, _ => ShouldNotHappen(loc), NotSucceeded(_)(loc))

  def shouldFail(e: E): P[Unit] =
    should(_ == Left(e), NotError(_, e)(loc), OtherError(_, e)(loc))

  def shouldBe(a: A): P[Unit] =
    should(_ == Right(a), NotEqualTo(_, a)(loc), NotValue(_, a)(loc))

}
