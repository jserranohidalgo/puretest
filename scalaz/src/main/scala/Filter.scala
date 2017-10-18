package org.hablapps.puretest

import scalaz.{Monad, MonadError, StateT}
import scalaz.syntax.monadError._

trait Filter[F[_]] {
  def filter[A](fa: F[A])(f: A => Boolean)(implicit
    F: sourcecode.File,
    L: sourcecode.Line): F[A]
}

object Filter extends LowerFilterImplicits {

  def apply[F[_]](implicit S: Filter[F]) = S

  // Use in for-comprehensions
  trait Syntax {

    implicit class FilterOps[F[_],A](fa: F[A])(implicit SF: Filter[F]){
      def filter(f: A => Boolean)(implicit F: sourcecode.File, L: sourcecode.Line): F[A] =
        SF.filter(fa)(f)
      def withFilter(f: A => Boolean)(implicit F: sourcecode.File, L: sourcecode.Line): F[A] =
        filter(f)
    }
  }

  object syntax extends Syntax

  case class LocationException(obtained: String, location: Location) extends Throwable {
    override def toString =
      s"$obtained does not match pattern at ${simplifyLocation(location)}"
    override def getMessage = toString
  }

  def FilterForMonadError[F[_], E](error: (String, Location) => E)(implicit
      merror: MonadError[F, E]) =
    new Filter[F] {
      def filter[A](fa: F[A])(f: A => Boolean)(implicit
        F: sourcecode.File, L: sourcecode.Line): F[A] =
        merror.bind(fa)(a =>
          if (f(a)) a.point[F] else merror.raiseError(error(a.toString, (F, L)))
        )
    }

  implicit def FilterForLocation[F[_]](
      implicit merror: MonadError[F, Location]) =
    FilterForMonadError[F, Location]((_, loc) => loc)

  implicit def FilterForThrowable[F[_]](
      implicit merror: MonadError[F, Throwable]) =
    FilterForMonadError[F, Throwable](LocationException(_, _))

}

trait LowerFilterImplicits {

  implicit def FilterEither[E] = new Filter[Either[E, ?]] {
    def filter[A](fa: Either[E, A])(f: A => Boolean)(implicit F: sourcecode.File, L: sourcecode.Line): Either[E, A] =
      fa match {
        case valid @ Right(a) if (f(a)) => valid
        case l @ Left(_) => l
        case Right(a) => throw FilterError(a.toString)((F, L))
      }
  }

  implicit def FilterStateT[F[_]: Monad: Filter, S, E] = new Filter[StateT[F, S, ?]] {
    def filter[A](fa: StateT[F, S, A])(f: A => Boolean)(implicit F: sourcecode.File, L: sourcecode.Line): StateT[F, S, A] =
      StateT[F, S, A] { bs =>
        val res = fa.run(bs)
        Filter[F].filter(res.map(_._2))(f).flatMap(x => res.map(y => (y._1, x)))
      }
  }

}
