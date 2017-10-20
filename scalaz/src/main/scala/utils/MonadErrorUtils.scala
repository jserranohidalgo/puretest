package org.hablapps.puretest

import scalaz.MonadError, scalaz.syntax.monadError._

trait MonadErrorUtils{

  implicit class MonadErrorOps[P[_],A](self: P[A]){
    def attempt[E: MonadError[P, ?]]: P[Either[E, A]] =
      (self map (Right(_): Either[E, A]))
        .handleError(error => (Left(error): Either[E,A]).pure[P])
  }

}