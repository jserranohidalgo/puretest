package org.hablapps.puretest

import scalaz.{Functor, Monad, MonadError}
import scalaz.syntax.monadError._

class PureMatchers[P[_], A](self: P[A]) {

  def attempt[E: MonadError[P, ?]]: P[Either[E, A]] =
    (self map (Right(_): Either[E, A]))
      .handleError(error => (Left(error): Either[E,A]).pure[P])

  def should[E](condition: Either[E,A] => Boolean,
    errorIfSuccess: A => PureTestError[E],
    errorIfFailure: E => PureTestError[E])(implicit
    ME: MonadError[P, PureTestError[E]]): P[Unit] =
    attempt(PureTestError.toMonadErrorApp).map{ result =>
      if (condition(result)) ().pure[P]
      else ME.raiseError(result.fold(errorIfFailure,errorIfSuccess))
    }

  def shouldFail[E](implicit ME: MonadError[P, PureTestError[E]], loc: Location): P[Unit] =
    should[E](_.isLeft, NotFailed(_)(loc), _ => ShouldNotHappen(loc))

  def shouldSucceed[E](implicit AE: MonadError[P, PureTestError[E]], loc: Location): P[Unit] =
    should[E](_.isRight, _ => ShouldNotHappen(loc), NotSucceeded(_)(loc))

  def shouldFail[E](e: E)(implicit ME: MonadError[P, PureTestError[E]], loc: Location): P[Unit] =
    should[E](_ == Left(e), NotError(_, e)(loc), OtherError(_, e)(loc))

  def shouldBe[E](a: A)(implicit AE: MonadError[P, PureTestError[E]], loc: Location): P[Unit] =
    should[E](_ == Right(a), NotEqualTo(_, a)(loc), NotValue(_, a)(loc))

}
