package org.hablapps

package object puretest
  extends StateTMonadError
  with StateTEqual
  with StateTArbitrary
  with StateValidationMonad
  with MonadErrorUtils{

  type Location = (sourcecode.File, sourcecode.Line)

  implicit def loc(implicit f: sourcecode.File, l: sourcecode.Line) = (f,l)

  /* matchers and ops */

  import scalaz.MonadError

  implicit def toPureMatchers[P[_], A, E](self: P[A])(implicit
    ME: MonadError[P, PureTestError[E]],
    loc: Location) = new PureMatchers(self)

  implicit def toBooleanOps[P[_]](p: P[Boolean]) =
    new BooleanOps(p)

}
