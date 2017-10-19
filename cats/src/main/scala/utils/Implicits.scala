package org.hablapps.puretest

import cats.{Applicative, FlatMap, MonadError, ~>}
import cats.data.StateT

object Implicits {

  implicit def unliftPuretestErrorMonadError[P[_], E](implicit ME: MonadError[P, PuretestError[E]]) =
    new MonadError[P, E] {
      def pure[A](x: A): P[A] = ME.pure(x)
      def flatMap[A, B](fa: P[A])(f: A => P[B]): P[B] = ME.flatMap(fa)(f)
      def tailRecM[A, B](a: A)(f: A => P[Either[A, B]]): P[B] = ME.tailRecM(a)(f)

      def handleErrorWith[A](fa: P[A])(f: E => P[A]): P[A] = ME.recoverWith(fa) {
        case ApplicationError(e) => f(e)
      }
      def raiseError[A](e: E): P[A] = ME.raiseError(ApplicationError(e))
    }

}
