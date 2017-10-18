package org.hablapps.puretest

import cats.MonadError
import cats.syntax.applicative._

trait Filter[F[_]]{
  def filter[A](fa: F[A])(f: A => Boolean)(implicit
    F: sourcecode.File,
    L: sourcecode.Line): F[A]
}

object Filter{

  def apply[F[_]](implicit S: Filter[F]) = S

  // Use in for-comprehensions
  trait Syntax{

    implicit class FilterOps[F[_],A](fa: F[A])(implicit SF: Filter[F]){
      def filter(f: A => Boolean)(implicit F: sourcecode.File, L: sourcecode.Line): F[A] =
        SF.filter(fa)(f)
      def withFilter(f: A => Boolean)(implicit F: sourcecode.File, L: sourcecode.Line): F[A] =
        filter(f)
    }
  }

  object Syntax extends Syntax

  case class LocationException(obtained: String, location: Location) extends Throwable {
    override def toString =
      s"returned value $obtained does not match pattern at ${location.toString}"
    override def getMessage = toString
  }

  def FilterForMonadError[F[_], E](error: (String, Location) => E)(implicit
      ME: MonadError[F, E]) =
    new Filter[F]{
      def filter[A](fa: F[A])(f: A => Boolean)(implicit
        F: sourcecode.File, L: sourcecode.Line): F[A] =
        ME.flatMap(fa)(a =>
          if (f(a)) a.pure[F] else ME.raiseError(error(a.toString,(F,L)))
        )
    }

  implicit def FilterForLocation[F[_]: MonadError[?[_], Location]] =
    FilterForMonadError((_, loc) => loc)

  implicit def FilterForThrowable[F[_]: MonadError[?[_], Throwable]] =
    FilterForMonadError[F, Throwable](LocationException(_, _))
}
