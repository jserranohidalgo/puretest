package org.hablapps.puretest

import scalaz.{Functor, MonadError}
import scalaz.syntax.monadError._

/**
 * Utilities for test specifications
 */
trait TestingOps {

  implicit class TestingOps[P[_], A](self: P[A]) {

    // Boolean version

    def fails[E](implicit ME: MonadError[P, E]): P[Boolean] =
      (self as false) handleError { _ => true.point[P] }

    def succeeds[E](implicit ME: MonadError[P, E]): P[Boolean] =
      (self as true) handleError { _ => false.point[P] }

    def isError[E: MonadError[P, ?]](e: E): P[Boolean] =
      (self as false).handleError {
        error: E => (error == e).point[P]
      }

    def isEqual(a: A)(implicit F: Functor[P]): P[Boolean] =
      self map (_ == a)

    def inspect[E: MonadError[P, ?]]: P[Either[E, A]] =
      (self map (Right(_): Either[E, A])) handleError {
        _.point[P] map Left.apply[E, A]
      }

    // Unit version

    def fails[E](implicit ME: MonadError[P, E], F: sourcecode.File, L: sourcecode.Line): P[Unit] =
      self map { a =>
        throw NotFailed(a)((F, L))
      } handleError { _ => ().point[P] }

    def succeeds[E](implicit ME: MonadError[P, E], F: sourcecode.File, L: sourcecode.Line): P[Unit] =
      self.void handleError { e => throw NotSucceeded(e)((F, L)) }

    def isError[E](e: E)(implicit ME: MonadError[P, E], F: sourcecode.File, L: sourcecode.Line): P[Unit] =
      self map { a => throw NotError(a, e)((F, L)) } handleError {
        case `e` => ().point[P]
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
      }) handleError {
        case `e` => ().pure[P]
        case other => ME.raiseError(OtherError(other, e)((F, L)))
      }

    def shouldBe(a1: A)(implicit ME: MonadError[P, Throwable],
        F: sourcecode.File, L: sourcecode.Line): P[Unit] =
      self >>= { a2 =>
        if (a1 == a2) ().point[P] else ME.raiseError(NotEqualTo(a1, a2)((F,L)))
      }
  }
}
