package org.hablapps.puretest

import cats.{ApplicativeError, Functor, Monad, MonadError}
import cats.syntax.all._

/**
 * Utilities for test specifications
 */
trait TestingOps {

  implicit class TestingOps[P[_], A](self: P[A]) {

    // Boolean version

    def fails[E](implicit ME: ApplicativeError[P, E]): P[Boolean] =
      (self as false) handleError { _ => true }

    def succeeds[E](implicit ME: ApplicativeError[P, E]): P[Boolean] =
      (self as true) handleError { _ => false }

    def isError[E: ApplicativeError[P, ?]](e: E): P[Boolean] =
      (self as false) handleError { _ == e }

    def isEqual(a: A)(implicit F: Functor[P]): P[Boolean] =
      self map (_ == a)

    def inspect[E: ApplicativeError[P, ?]]: P[Either[E, A]] =
      (self map (Right(_): Either[E, A])).handleError(Left.apply[E, A])

    // Unit version

    def fails[E](implicit ME: ApplicativeError[P, E], F: sourcecode.File, L: sourcecode.Line): P[Unit] =
      self map { a =>
        throw NotFailed(a)((F, L))
      } handleError { _ => () }

    def succeeds[E](implicit ME: ApplicativeError[P, E], F: sourcecode.File, L: sourcecode.Line): P[Unit] =
      self.void handleError { e => throw NotSucceeded(e)((F, L)) }

    def isError[E](e: E)(implicit AE: ApplicativeError[P, E], F: sourcecode.File, L: sourcecode.Line): P[Unit] =
      self map { a => throw NotError(a, e)((F, L)) } handleError {
        case `e` => ()
        case other => throw OtherError(other, e)((F, L))
      }

    def isEqual(a2: A)(implicit Fu: Functor[P], F: sourcecode.File, L: sourcecode.Line): P[Unit] =
      self map { a1 =>
        if (a1 == a2) ()
        else throw NotEqualTo(a1, a2)((F, L))
      }

    // @deprecated Throwable version

    def shouldFail(e: Throwable)(implicit ME: MonadError[P, Throwable],
        F: sourcecode.File, L: sourcecode.Line): P[Unit] =
      (self >>= { a =>
        ME.raiseError[Unit](NotError(a, e)((F, L)))
      }) handleErrorWith {
        case `e` => ().pure[P]
        case other => ME.raiseError(OtherError(other, e)((F, L)))
      }

    def shouldBe(a2: A)(implicit ME: MonadError[P, Throwable],
        F: sourcecode.File, L: sourcecode.Line): P[Unit] =
      self >>= { a1 =>
        if (a1 == a2) ().pure[P]
        else ME.raiseError(NotEqualTo(a1, a2)((F, L)))
      }

  }
}

