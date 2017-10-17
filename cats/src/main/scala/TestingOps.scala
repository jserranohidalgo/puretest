package org.hablapps.puretest

import cats.{ApplicativeError, Functor, MonadError}
import cats.syntax.all._

/**
 * Utilities for test specifications
 */
trait TestingOps {

  implicit class TestingOps[P[_], A](self: P[A]) {

    def isError[E: ApplicativeError[P, ?]](e: E): P[Boolean] =
      (self as false) handleError { _ == e }

    def inspect[E: ApplicativeError[P, ?]]: P[Either[E, A]] =
      (self map (Right(_): Either[E, A])).handleError(Left.apply[E, A])

    def isEqual(a: A)(implicit F: Functor[P]): P[Boolean] =
      self map (_ == a)

    def shouldFail(e: Throwable)(implicit ME: MonadError[P, Throwable],
        F: sourcecode.File, L: sourcecode.Line): P[Unit] =
      self.void handleErrorWith {
        case `e` => ().pure[P]
        case other => ME.raiseError(OtherError(other, e)((F, L)))
      } >>= { a =>
        ME.raiseError(NotError(a, e)((F, L)))
      }

    def shouldBe(a2: A)(implicit ME: MonadError[P, Throwable],
        F: sourcecode.File, L: sourcecode.Line): P[Unit] =
      self >>= { a1 =>
        if (a1 == a2) ().pure[P]
        else ME.raiseError(NotEqualTo(a1, a2)((F, L)))
      }

  }
}

