package org.hablapps.puretest

import scalaz.{Functor, MonadError}
import scalaz.syntax.monadError._

/**
 * Utilities for test specifications
 */
trait TestingOps {

  implicit class TestingOps[P[_], A](self: P[A]) {

    def isError[E: MonadError[P,?]](e: E): P[Boolean] =
      (self as false).handleError{
        error: E => (error == e).point[P]
      }

    def inspect[E: MonadError[P, ?]]: P[Either[E, A]] =
      (self map (Right(_): Either[E,A])) handleError {
        _.point[P] map Left.apply[E,A]
      }

    def isEqual(a: A)(implicit F: Functor[P]): P[Boolean] =
      F.map(self)(_ == a)

    def shouldFail(e: Throwable)(implicit ME: MonadError[P, Throwable],
        F: sourcecode.File, L: sourcecode.Line): P[Unit] =
      self.void handleError {
        case `e` => ().pure[P]
        case other => ME.raiseError(OtherError(other, e)((F, L)))
      } >>= { a =>
        ME.raiseError(NotError(a, e)((F, L)))
      }

    def shouldBe(a1: A)(implicit ME: MonadError[P, Throwable],
        F: sourcecode.File, L: sourcecode.Line): P[Unit] =
      self >>= { a2 =>
        if (a1 == a2) ().point[P] else ME.raiseError(NotEqualTo(a1, a2)((F,L)))
      }
  }
}
