package org.hablapps.puretest

import scalaz.{Monad, MonadError, StateT}
import scalaz.syntax.monadError._

trait Filter[F[_]] {
  def filter[A](fa: F[A])(f: A => Boolean)(implicit
    F: sourcecode.File,
    L: sourcecode.Line): F[A]
}

object Filter{

  def apply[F[_]](implicit S: Filter[F]) = S

  object syntax {

    implicit class FilterOps[F[_],A](fa: F[A])(implicit SF: Filter[F]){
      def filter(f: A => Boolean)(implicit F: sourcecode.File, L: sourcecode.Line): F[A] =
        SF.filter(fa)(f)
      def withFilter(f: A => Boolean)(implicit F: sourcecode.File, L: sourcecode.Line): F[A] =
        filter(f)
    }
  }

  implicit def FilterForMonadError[F[_], E](implicit
      merror: MonadError[F, PureTestError[E]]) =
    new Filter[F] {
      def filter[A](fa: F[A])(f: A => Boolean)(implicit
        F: sourcecode.File, L: sourcecode.Line): F[A] =
        fa shouldMatch f
    }

}