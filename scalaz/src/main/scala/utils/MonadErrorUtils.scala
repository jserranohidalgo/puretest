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

  import shapeless._
  implicit def toMonadError[P[_],E1,E2](implicit 
    toE1: E2 => Option[E1],
    toE2: E1 => E2,
    ME: Lazy[MonadError[P,E2]]) = 
    new MonadError[P,E1]{
      def point[A](a: => A) = ME.value.point(a)
      def bind[A,B](p: P[A])(f: A => P[B]) = ME.value.bind(p)(f)
      def raiseError[A](e: E1) =
        ME.value.raiseError(toE2(e))
      def handleError[A](p: P[A])(f: E1 => P[A]) =
        ME.value.handleError(p){ e2 => toE1(e2) match {
          case Some(e1) => f(e1)
          case None => ME.value.raiseError(e2)
        }}
    }


}