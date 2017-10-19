package org.hablapps.puretest

import scalaz.{Functor, Monad, MonadError}
import scalaz.syntax.monadError._

class PureMatchers[P[_], A](self: P[A]) {

  def shouldFail[E](implicit ME: MonadError[P, PureTestError[E]], F: sourcecode.File, L: sourcecode.Line): P[Unit] =
    (self >>= { a =>
      ME.raiseError[Unit](NotFailed(a)((F, L)))
    }) handleError { _ => ().point[P] }

  def shouldSucceed[E](implicit AE: MonadError[P, PureTestError[E]], F: sourcecode.File, L: sourcecode.Line): P[A] =
    self handleError {
      case ApplicationError(e) => AE.raiseError(NotSucceeded(e)((F, L)))
      case other => AE.raiseError(other)
    }

  def shouldFail[E](e: E)(implicit ME: MonadError[P, PureTestError[E]], F: sourcecode.File, L: sourcecode.Line): P[Unit] =
    (self >>= { a =>
      ME.raiseError[Unit](NotError(a, e)((F, L)))
    }) handleError {
      case ApplicationError(`e`) => ().pure[P]
      case ApplicationError(other) => ME.raiseError(OtherError[E](other, e)((F, L)))
      case other => ME.raiseError(other)
    }

  def shouldBe[E](a2: A)(implicit AE: MonadError[P, PureTestError[E]], F: sourcecode.File, L: sourcecode.Line): P[Unit] =
    self >>= { a1 =>
      if (a1 == a2) ().pure[P]
      else AE.raiseError(NotEqualTo(a1, a2)((F, L)))
    }
}
