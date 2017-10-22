package org.hablapps.puretest

import scalaz.MonadError, scalaz.syntax.monadError._

trait MonadErrorUtils{

  trait RaiseError[P[_],E]{
    def raiseError[A](e: E): P[A]
  }

  object RaiseError{
    def apply[P[_],E](implicit RE: RaiseError[P,E]) = RE

    implicit def fromMonadError[P[_],E](implicit ME: MonadError[P,E]) = 
      new RaiseError[P,E]{
        def raiseError[A](e: E) = ME.raiseError(e)
      }
  }

  implicit class MonadErrorOps[P[_],A](self: P[A]){
    def attempt[E: MonadError[P, ?]]: P[Either[E, A]] =
      (self map (Right(_): Either[E, A]))
        .handleError(error => (Left(error): Either[E,A]).pure[P])
  }
}