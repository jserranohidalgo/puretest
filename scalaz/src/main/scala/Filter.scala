package org.hablapps.puretest

trait Filter[F[_]]{
  def filter[A](fa: F[A])(f: A => Boolean)(implicit
    F: sourcecode.File,
    L: sourcecode.Line): F[A]
}

object Filter{

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

  import scalaz.MonadError, scalaz.syntax.monadError._

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

  // POC(jfuentes)
  implicit def FilterForTester[F[_], E](implicit
      T: Tester[F, E],
      ME: MonadError[F, Either[LocationException, E]]) =
    new Filter[F] {
      def filter[A](fa: F[A])(f: A => Boolean)(implicit
          F: sourcecode.File, L: sourcecode.Line): F[A] =
        fa >>= { a =>
          if (f(a)) a.point[F]
          else ME.raiseError(Left(LocationException(a.toString, (F, L))))
        }
    }

}
