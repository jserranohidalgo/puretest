package org.hablapps.puretest

import scalaz.{MonadError, StateT, IndexedStateT}

trait StateTMonadError extends LowerPriorityImplicits{


  implicit def stateTMonadError[S, F[_], E](implicit F: MonadError[F, E]) =
    new MonadError[StateT[F, S, ?], E]{

      import scalaz.Need

      def point[A](a: => A): StateT[F, S, A] = {
        val aa = Need(a)
        StateT(s => F.point(s, aa.value))
      }

      def bind[A, B](fa: StateT[F, S, A])(f: A => StateT[F, S, B]): StateT[F, S, B] = fa.flatMap(f)

      def raiseError[A](e: E): StateT[F,S,A] =
        IndexedStateT(_ => F.raiseError(e))

      def handleError[A](fa: StateT[F,S,A])(
        f: E => StateT[F,S,A]): StateT[F,S,A] =
          fa.mapsf(sf => (s: S) =>
            F.handleError(sf(s)){ e =>
              val fe: F[S=>F[(S,A)]] = f(e).getF(F)
              F.bind(fe)(ff => ff(s))
            }
          )
    }
}

trait LowerPriorityImplicits{

  implicit def toMonadError[P[_],E](implicit 
    toE: PureTestError[E] => Option[E],
    fromE: E => PureTestError[E],
    ME: MonadError[P,PureTestError[E]]) = 
    new MonadError[P,E]{
      def point[A](a: => A) = ME.point(a)
      def bind[A,B](p: P[A])(f: A => P[B]) = ME.bind(p)(f)
      def raiseError[A](e: E) =
        ME.raiseError(fromE(e))
      def handleError[A](p: P[A])(f: E => P[A]) =
        ME.handleError(p){ e2 => toE(e2) match {
          case Some(e1) => f(e1)
          case None => ME.raiseError(e2)
        }}
    }

}